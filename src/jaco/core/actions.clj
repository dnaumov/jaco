(ns jaco.core.actions)

(def ^{:doc "TODO: write"} ^:dynamic *request*)
(def ^{:doc "TODO: write"} ^:dynamic *error-handler* nil)
(def ^{:doc "TODO: write"} ^:dynamic *errors*)


(defmulti make-param-fn
  "Given name (a keyword) and any number of arguments, returns appropriate
   validator or converter."
  (fn [name & args] name))

(defn validator [f]
  (fn [x] {:passed? (try (boolean (f x))
                         (catch Exception _ false))
           :val x}))

(defn converter [f]
  #(try {:passed? true :val (f %)}
        (catch Exception _
          {:passed? false :val %})))

(defmacro defvalidator
  "TODO: write"
  [name params body]
  `(defmethod make-param-fn ~name [_# ~@params]
     (validator ~body)))

(defmacro defconverter
  "TODO: write"
  [name params body]
  `(defmethod make-param-fn ~name [_# ~@params]
     (converter ~body)))


(defn canonize [checkers]
  (->> checkers
       (reduce (fn [acc next]
                 ((if (map? next) into conj) acc next)) [])
       (map #(if (keyword? %) [%] (vec %)))
       vec))

(defn transform-params-list [coll] ;=> ([sym [checker args*]*] ...)
  (->> coll
       (reduce (fn [[acc checkers] next]
                 (if (symbol? next)
                   [(conj acc [next checkers]) []]
                   [acc (conj checkers next)]))
               [[] []])
       first 
       (map (fn [[sym checkers]]
              (if-let [t (-> sym meta :tag)]
                [sym t]
                [sym (canonize checkers)])))
       (into {})))

(defn process-param [sym val checkers] ;=> [sym new-val failed-checkers]
  (letfn [(f [[val failed-checkers] checker-name+args]
            (let [checker (apply make-param-fn checker-name+args)
                  {:keys [passed? val]} (checker val)]
              [val (if passed?
                     failed-checkers
                     (conj failed-checkers checker-name+args))]))]
    (cons sym (reduce f [val []] checkers))))

(defn- wrap-processing [params body]
  (let [syms+vals (apply concat
                         (map (fn [[sym checkers]]
                                [sym `(process-param '~sym ~sym ~checkers)])
                              params))]
    `(let [~@syms+vals] ~body)))

(defn- wrap-error-handler [{:keys [error-handler]} body]
  (if (not= error-handler :default)
    `(binding [*error-handler* ~error-handler]
       ~body)
    body))

(defn- wrap-errors [params body]
  (let [binds (apply concat (map #(list % `(second ~%)) params))]
    `(binding [*errors* (seq (filter (comp seq last) [~@params]))]
       (if (and *error-handler* *errors*)
         (*error-handler* *errors*)
         (let [~@binds]
           ~body)))))


(defmacro action
  "TODO: write"
  {:arglists '([opts? params & body])} [opts-or-params & more]
  (let [[opts params body] (if (map? opts-or-params)
                             [opts-or-params (first more) (rest more)]
                             [{} opts-or-params more])
        opts (merge {:error-handler :default} opts)
        params (transform-params-list params)
        params-symbols (map first params)
        body (cons 'do body)]
    `(fn [{:keys [~@params-symbols]}]
       ~(->> body
             (wrap-errors params-symbols)
             (wrap-error-handler opts)
             (wrap-processing params)))))



(do
  (defvalidator :max-length [x] #(<= (count %) x))
  (defvalidator :min-length [x] #(>= (count %) x))
  (defvalidator :equal [x] #(= % x))
  (defvalidator :matches? [re] #(boolean (re-matches re %)))

  (defvalidator :num? [] number?)
  (defvalidator :pos? [] pos?)
  (defconverter :inc [] inc)
  (defconverter :int [] #(Integer/parseInt %))
  (defconverter :make-hash [] hash)
  )

