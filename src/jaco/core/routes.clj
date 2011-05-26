(ns jaco.core.routes
  (:refer-clojure :exclude [error-handler])
  (:require [compojure.core  :as compojure])
  (:use [clojure.contrib.def :only [name-with-attributes]]
        [jaco.core.actions   :only [*request* *error-handler*]]
        [clojure.string      :only [join]]
        [clout.core          :only [route-compile]]))

(defn encode [s]
  (java.net.URLEncoder/encode (str s)))

(defn- path-args [path]
  (let [path-seq (rest (.split path "/|\\." -1))
        params (filter #(.startsWith % ":") path-seq)]
    (map #(symbol (apply str (rest %))) params)))

(defn- url-constructor [path args get-params]
  `(str
    (-> ~path
        ~@(for [a args]
            `(.replace (str ":" '~a) (encode ~a))))
    (when-let [get-params# (seq ~get-params)]
      (str "?" (join "&" (map #(str %1 "=" %2)
                              (map (comp encode name key) get-params#)
                              (map (comp encode val) get-params#)))))))

(defn- make-url-fn [name path opts]
  (let [path (.replace path "*" ":*")
        make-path `(str (::context-path (meta (var ~name))) ~path)
        args (path-args path)
        more (gensym "more")]
    `(fn [~@args & ~more]
       ~@(for [a args]
           `(when-let [regex# ((keyword '~a) ~opts)]
              (when-not (re-matches regex# (str ~a))
                (throw (IllegalArgumentException. (str "Param not match the regex: " regex#))))))
       (when-not (even? (count ~more))
         (throw (IllegalArgumentException. "An even number of additional args required")))
       ~(url-constructor make-path args `(apply array-map ~more)))))

(defmacro defroute
  {:arglists '([name path opts?])}
  [name path & [opts]]
  (let [arglists (list 'quote (list (vec (concat (path-args path) '(& get-params)))))
        name (with-meta name {:arglists arglists
                              ::path path ::opts opts})]
    `(def ~name ~(make-url-fn name path opts))))


(defmacro ^{:private true} make-route
  [method path opts fns]
  (let [req-sym (gensym "request")
        path `(route-compile ~path (or ~opts {}))
        body `(binding [*request* ~req-sym]
                ((apply comp (reverse ~fns)) (:params ~req-sym)))]
    (#'compojure/compile-route method path req-sym [body])))

(defn route
  {:arglists '([route-var method? f & fs])} [& more]
  (let [{path ::path, opts ::opts} (meta (first more))
        [method fns] (if (keyword? (second more))
                       [(second more) (drop 2 more)]
                       [:get (rest more)])]
    (make-route method path opts fns)))


(defn set-context-path! [route-var path]
  (alter-meta! route-var assoc ::context-path path))

(defmacro context [path & handlers]
  `(let [route+ctxt# (map #(update-in % [1] (partial str ~path))
                          (apply merge (map #(-> % meta ::routes) [~@handlers])))]
     (do
       (doseq [[route# ctxt#] route+ctxt#]
         (set-context-path! route# ctxt#))
       (with-meta (compojure/context ~path [] ~@handlers)
         {::routes (into {} route+ctxt#)}))))


(defn- bind-err-handler [f]
  `(fn [handler#]
     (fn [req#]
       (binding [*error-handler* ~f]
         (handler# req#)))))

(defn- routes-meta [routes]
  (let [named-route? #(and (coll? %) (= (first %) 'route))]
    `(merge
      ~(into {} (map (fn [[_ route-var]] [route-var nil])
                     (filter named-route? routes)))
      ~@(map (fn [x] `(::routes (meta ~x)))
             (filter (complement named-route?) routes)))))

(defmacro defroutes [name & more]
  (let [[name routes] (name-with-attributes name more)
        {:keys [error-handler middlewares]} (meta name)
        middlewares (reverse (if error-handler
                               (conj middlewares (bind-err-handler error-handler))
                               middlewares))
        name (with-meta name (dissoc (meta name) :error-handler :middlewares))]
    `(def ~name (with-meta ~(if (seq middlewares)
                              `((comp ~@middlewares) (compojure/routes ~@routes))
                              `(compojure/routes ~@routes))
                  {::routes ~(routes-meta routes)}))))
