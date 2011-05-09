(ns jaco.crud.actions
  (:use [jaco.core.actions :only [*request* *errors* defaction action converter]])
  (:require (jaco.crud [datasource :as ds]
                       [templates  :as tpl]
                       [routes     :as routes])))

(def *crud-map* {})

(defn- crud-props [type]
  (or (get *crud-map* type)
      (throw (IllegalArgumentException. (str "There is no CRUD for the type " type)))))

;; handlers, defcrud creates their impl
(defn- dispatch [type params] type)

(defmulti create dispatch)
(defmulti update dispatch)
(defmulti delete dispatch)


;;================================
;; Actions
;;================================

(def type [:type (converter #(Class/forName (.replaceAll % ":" "."))
                            (constantly "There is no class with the given name."))])

(def id [:id (converter #(and (seq %) (Integer/parseInt %)) 
                        (constantly "Wrong entity ID."))])

(defn- apply-to-string [entity]
  (into {}
        (for [[k v] entity]
          [k ((-> *crud-map* (get (class entity)) :fields k :to-string) v)])))

;; :view - fn of 2 args, elem name and its value
(defmacro replace-view [type expr]
  (let [k 'k, m 'm]
    `(map (fn [[~k ~m]]
            (-> ~m
                (assoc :view ((:view ~m) (name ~k) ~expr))
                (dissoc :default :to-string)))
          (:fields (crud-props ~type)))))


;; :default - fn of no args, which used to get default value when creating new entity
(defaction create-page [type]
  (tpl/view nil (replace-view type ((:default m)))))

(defaction update-page [type id]
  (when-let [entity (ds/retrieve type id)]
    (tpl/view id (replace-view type (k (apply-to-string entity))))))

(defaction overview [type]
  (tpl/overview (map #(vector (routes/update (.getName type) (ds/get-id %))
                              (routes/delete (.getName type) (ds/get-id %))
                              (select-keys (apply-to-string %) (:overview (crud-props type))))
                     (ds/retrieve-all type))
                (routes/create (.getName type))))

(defaction index []
  (tpl/index (map (fn [[k v]]
                    {:title (:title v)
                     :url (routes/overview (.getName k))})
                  *crud-map*)))


;;================================
;; The Macro
;;================================

(defrecord Field [title comment view default to-string])

(defn provide-default-opts [entity-type fields]
  {:title (str entity-type)
   :overview (map first fields)
   :factory (fn [_]
              (throw (UnsupportedOperationException. "Factory fn is not provided")))
   :create (action [type]
            (ds/save! ((:factory (get *crud-map* type)) (:params *request*)))
            (tpl/completed))
   :update (action [type id]
             (when-let [entity (ds/retrieve type id)]
               (ds/save! (merge entity
                                (select-keys (:params *request*) (keys entity))))
               (tpl/completed)))
   :delete (action [type id]
             (ds/delete! (ds/retrieve type id))
             (tpl/completed))})

(defn alter-crud-map
  [type {:keys [title overview factory]} fields]
  `(alter-var-root (var *crud-map*) assoc ~type
                   {:title ~title
                    :overview ~(vec (map keyword overview))
                    :factory ~factory
                    :fields ~(into {} (map (fn [[name title & {:keys [comment view default to-string]}]]
                                             [(keyword name)
                                              (Field. title comment
                                                      (or view tpl/default-view)
                                                      (or default str)
                                                      (or to-string str))])
                                           fields))}))

(defmacro defcrud
  {:arglists '([type opts? & fields])}
  [type & opts+fields]
  (let [[opts fields] (if (map? (first opts+fields))
                        [(first opts+fields) (rest opts+fields)]
                        [nil opts+fields])
        opts (merge (provide-default-opts type fields) opts)]
    `(do
       ~(alter-crud-map type opts fields)
       (defmethod create ~type [type# params#] (~(:create opts) params#))
       (defmethod update ~type [type# params#] (~(:update opts) params#))
       (defmethod delete ~type [type# params#] (~(:delete opts) params#)))))
