(ns jaco.core.actions)

(def ^{:doc "TODO: write"} *request*)
(def ^{:doc "TODO: write"} *error-handler* nil)
(def ^{:doc "TODO: write"} *errors*)

(defrecord Param [key val errors])

(defn- wrap-do [body]
  (if (next body) (cons 'do body) (first body)))

(defn- wrap-let [keys+vals body]
  (let [binds (apply concat (map (fn [[k v]] [(symbol (name k)) v])
                                 keys+vals))]
    `(let [~@binds]
       ~body)))

;; params-sym here will contain map of [:param-key Param-record]
(defn- args-for-let [params-sym args]
  (into {} (map #(if (symbol? %)
                   [% `(:val ((first ~%) ~params-sym))]
                   [(-> % first name symbol) `(-> ~params-sym ~(first %) :val)])
                args)))

;; *error-handler* can not be rebound when set in this way
(defn- wrap-error-handler [{:keys [error-handler]} body]
  (if (not= error-handler :default)
    `(binding [*error-handler* ~error-handler]
       ~body)
    body))

(defn process-param [key val fns]
  (reduce (fn [val f] (f val))
          (Param. key val [])
          fns))

(defn process-all-params [params-map arg-vectors]
  (into {} (map (fn [[k v fns]] [k (process-param k v fns)])
                (for [[k & fns] arg-vectors]
                  [k (params-map k) fns]))))

;; re-lets params-sym with processed values
(defn- wrap-errors [params-sym args body]
  `(let [~params-sym (process-all-params ~params-sym ~args)]
     (binding [*errors* (seq (filter (comp seq :errors) (vals ~params-sym)))]
       (if (and *error-handler* *errors*)
         (*error-handler* *errors*)
         ~body))))


(defmacro action
  {:arglists '([opts? args & body])} [opts-or-args & more]
  (let [[opts args body] (if (map? opts-or-args)
                           [opts-or-args (first more) (rest more)]
                           [{} opts-or-args more])
        opts (merge {:error-handler :default} opts)
        args (vec (map #(if (keyword? %) [%] %) args))
        params (gensym "params")]
    `(fn [~params]
       ~(->> body
             wrap-do
             (wrap-let (args-for-let params args))
             (wrap-errors params args)
             (wrap-error-handler opts)))))

(defmacro defaction
  {:arglists '([name doc-string? opts? args & body])} [name doc-or-opts-or-args & more]
  (let [[doc opts-or-args more] (if (string? doc-or-opts-or-args)
                                  [doc-or-opts-or-args (first more) (rest more)]
                                  [nil doc-or-opts-or-args more])
        name (with-meta name (merge (meta name) {:doc doc}))]
    `(def ~name (action ~opts-or-args ~@more))))


;; returns a fn: Param -> Param
(defn validator
  [pred errfn & {:keys [ensure?] :or {ensure? false}}]
  (fn [{:keys [key val errors]}]
    (if (or (and ensure? (seq errors)) (pred val))
      (Param. key val errors)
      (Param. key val (conj errors (errfn key val errors))))))

(defn converter
  [f errfn & {:keys [ensure?] :or {ensure? false}}]
  (fn [{:keys [key val errors]}]
    (if (and ensure? (seq errors))
      (Param. key val errors)
      (try
        (Param. key (f val) errors)
        (catch Exception e
          (Param. key val (conj errors (errfn key val errors))))))))
