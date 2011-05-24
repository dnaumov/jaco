(ns jaco.core.routes
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
  `(do ~@(map (fn [h] `(doseq [r# (-> ~h var meta ::routes)]
                         (set-context-path! r# ~path)))
              handlers)
       (compojure/context ~path [] ~@handlers)))


(defmacro defroutes [name & more]
  (let [[name routes] (name-with-attributes name more)
        err-handler (-> name meta :error-handler)
        name (with-meta name (conj (dissoc (meta name) :error-handler)
                                   {::routes (vec (map second routes))}))]

    (if err-handler
      `(def ~name (fn [req#] (binding [*error-handler* ~err-handler]
                               ((compojure/routes ~@routes) req#))))
      `(def ~name (compojure/routes ~@routes)))))
