(ns jaco.core.routes
  (:require compojure.core)
  (:use [jaco.core.actions :only [*request*]]
        [clojure.string    :only [join]]
        [clout.core        :only [route-compile]]))

(defn encode [s]
  (java.net.URLEncoder/encode (str s)))

(defn url-args [path]
  (let [path-seq (rest (.split path "/|\\." -1))
        params (filter #(.startsWith % ":") path-seq)]
    (map #(symbol (apply str (rest %))) params)))

(defn url-constructor [path args get-params]
  `(str
    (-> ~path
        ~@(for [a args]
            `(.replace (str ":" '~a) (encode ~a))))
    (when-let [get-params# (seq ~get-params)]
      (str "?" (join "&" (map #(str %1 "=" %2)
                              (map (comp encode name key) get-params#)
                              (map (comp encode val) get-params#)))))))

(defn make-url-fn [name path opts]
  (let [path (.replace path "*" ":*")
        make-path `(str (::context-path (meta (var ~name))) ~path)
        args (url-args path)
        more (gensym "more")]
    `(fn [~@args & ~more]
       ~@(for [a args]
           `(when-let [regex# ((keyword '~a) ~opts)]
              (when-not (re-matches regex# ~a)
                (throw (IllegalArgumentException. (str "Param not match the regex: " regex#))))))
       (when-not (even? (count ~more))
         (throw (IllegalArgumentException. "An even number of additional args required")))
       ~(url-constructor make-path args `(apply array-map ~more)))))

(defmacro defroute
  {:arglists '([name path opts?])}
  [name path & [opts]]
  (let [arglists (list 'quote (list (vec (concat (url-args path) '(& get-params)))))
        name (with-meta name {:arglists arglists
                              ::path path ::opts opts})]
    `(def ~name ~(make-url-fn name path opts))))


(defmacro make-route [method path opts fns]
  (let [req-sym (gensym "request")
        path `(route-compile ~path (or ~opts {}))
        body `(binding [*request* ~req-sym]
                ((apply comp (reverse ~fns)) (:params ~req-sym)))]
    (#'compojure.core/compile-route method path req-sym [body])))


(defn route
  {:arglists '([route-var method? f & fs])} [& more]
  (let [{path ::path, opts ::opts} (meta (first more))
        [method fns] (if (keyword? (second more))
                       [(second more) (drop 2 more)]
                       [:get (rest more)])]
    (make-route method path opts fns)))


(defn set-context-path! [path & routes]
  (doseq [r routes]
    (alter-meta! r assoc ::context-path path)))

(defmacro context [& _]) ;; TODO: write -- 09.05.11
