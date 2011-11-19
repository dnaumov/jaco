(ns jaco.core.routes
  (:require [compojure.core  :as compojure])
  (:use [jaco.core.actions   :only [*request* *error-handler*]]
        [clojure.tools.macro :only [name-with-attributes]]
        [clojure.string      :only [join]]
        [clout.core          :only [route-compile]]))

;;; url generation

(defn encode [s]
  (java.net.URLEncoder/encode (str s)))

(defn- path-args [path]
  (let [path-seq (rest (.split path "/|\\." -1))
        params (filter #(.startsWith % ":") path-seq)]
    (map #(symbol (apply str (rest %))) params)))

(defn- url-constructor [path args get-params]
  `(let [{[anch#] true, get-params# false} (group-by #(= (key %) :#) ~get-params)
         str-when# (fn [test# & exprs#] (when test# (apply str exprs#)))]
     (str
      (-> ~path
          ~@(for [a args]
              `(.replace (str ":" '~a) (encode ~a))))
      (str-when# anch# "#" (second anch#))
      (str-when# (seq get-params#)
                 "?"
                 (join "&" (map #(str %1 "=" %2)
                                (map (comp encode name key) get-params#)
                                (map (comp encode val) get-params#)))))))

(defn- make-url-fn [name path opts]
  (let [path (.replace path "*" ":*")
        make-path `(str (:context ~name) ~path)
        args (path-args path)
        more (gensym "more")]
    `(fn [~@args & ~more]
       ~@(for [a args]
           `(when-let [regex# ((keyword '~a) ~opts)]
              (when-not (re-matches regex# (str ~a))
                (throw (IllegalArgumentException. (str "Param doesn't match the regex: " regex#))))))
       (when-not (even? (count ~more))
         (throw (IllegalArgumentException. "An even number of additional args required")))
       ~(url-constructor make-path args `(apply array-map ~more)))))


;;; defroute

(defmacro defrecord-impl-ifn
  "Defines record which implements clojure.lang.IFn with vararg function."
  [name fields f]
  (let [make-invoke #(let [xs (repeatedly % gensym)]
                       `(invoke [this# ~@xs] (~f ~@xs)))
        invokes (map make-invoke (range 20))]
    `(defrecord ~name ~fields
       clojure.lang.IFn
       ~@invokes)))

(defrecord-impl-ifn NamedRoute
  [actions views path opts gen-url-fn]
  gen-url-fn)

(defn named-route? [x] (instance? NamedRoute x))

(defmacro defroute
  "TODO: write"
  {:arglists '([name path opts?])}
  [name path & [opts]]
  (let [arglists (list 'quote (list (vec (concat (path-args path) '(& get-params)))))
        name (with-meta name {:arglists arglists
                              ::path path ::opts opts})]
    `(def ~name (NamedRoute. {} {} ~path ~opts ~(make-url-fn name path opts)))))


;;; view

(defmacro defaction ;; TODO: move to actions ns
  "TODO: write"
  {:arglists '([route method? args & body])}
  [route method-or-args & [m & ms :as more]]
  (let [[method args body] (if (keyword? method-or-args)
                             [method-or-args m ms]
                             [:any method-or-args more])]
    `(alter-var-root (var ~route) assoc-in [:actions ~method]
                     (fn [{:keys [~@args]}] ~@body))))

(def generic-views {})
(def ^:dynamic *output* nil)

(defmacro with-output [val & body]
  `(binding [*output* ~val] ~@body))

(defn defview* [route method output f]
  (if route
    (alter-var-root route assoc-in [:views method output] f)
    (alter-var-root #'generic-views assoc-in [method output] f)))


;;; defhandler

(defn combine-fns [fns]
  (apply comp (map #(fn [x] (when x (% x))) (reverse fns))))

(defmacro ^{:private true} make-route
  [method path opts fns]
  (let [req-sym (gensym "request")
        method `(when-not (= ~method :any) ~method)
        path `(route-compile ~path (or ~opts {}))
        body `(binding [*request* ~req-sym]
                ((combine-fns ~fns) (:params ~req-sym)))]
    (#'compojure/compile-route method path req-sym [body])))

(defn get-view [{:keys [views]} method output-format]
  (let [get* #(get %1 %2 (get %1 :any))
        f #(-> (get* % method)
               (get* output-format))]
    (or (f views)
        (f generic-views)
        (throw (IllegalArgumentException. "Can't find appropriate view.")))))

(defn construct-routes
  [{:keys [actions path opts] :as named-route}]
  (map (fn [[method action]]
         (make-route method path opts
                     [action (get-view named-route method *output*)]))
       (sort (comparator (fn [[m _] _] (not= m :any))) actions)))

(defn handler [xs]
  (let [{routes true, handlers false} (group-by named-route? xs)]
    (apply compojure/routes
           (concat handlers (flatten (map construct-routes routes))))))


(defn- bind-err-handler [f]
  `(fn [handler#]
     (fn [req#]
       (binding [*error-handler* ~f]
         (handler# req#)))))

(defn routes-meta [handlers]
  (apply merge
         (zipmap (filter var? handlers) (repeat nil))
         (map #(-> % meta ::routes)
              (remove var? handlers))))


(defmacro defhandler
  "TODO: write"
  [name & more]
  (let [[name routes] (name-with-attributes name more)
        {:keys [error-handler middleware]} (meta name)
        name (vary-meta name dissoc :error-handler :middleware)
        middleware (apply comp (reverse
                                (if error-handler
                                  (conj middleware (bind-err-handler error-handler))
                                  middleware)))
        var? #(and (coll? %) (= (first %) 'var))
        routes* (map #(if (var? %) (second %) %) routes)]
    `(def ~name (with-meta (~middleware (handler [~@routes*]))
                  {::routes (routes-meta [~@routes])}))))


;;; context

(defn set-context [route-var ctxt]
  (alter-var-root route-var assoc :context ctxt))

(defmacro context [path & handlers]
  `(let [route+ctxt# (map #(update-in % [1] (partial str ~path))
                          (apply merge (map #(-> % meta ::routes) [~@handlers])))]
     (do
       (doseq [[route# ctxt#] route+ctxt#]
         (set-context route# ctxt#))
       (with-meta (compojure/context ~path [] ~@handlers)
         {::routes (into {} route+ctxt#)}))))
