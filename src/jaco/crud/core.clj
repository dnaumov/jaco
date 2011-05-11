(ns jaco.crud.core
  (:use jaco.crud.routes
        [compojure.core     :only [defroutes]]
        (jaco.core [routes  :only [route]]
                   [actions :only [*error-handler*]]))
  (:require (jaco.crud [actions   :as actions]
                       [templates :as templates])))

(defroutes crud-routes {:private true}
  (route #'index    actions/index)
  (route #'overview actions/overview)
  (route #'create   actions/create-page)
  (route #'update   actions/update-page)

  (route #'create :post actions/create)
  (route #'update :post actions/update)
  (route #'delete :get  actions/delete))

;; Compojure uses dot as a separator, so we need something like this
(defn- middleware [handler]
  (fn [req]
    (binding [*error-handler* #(templates/error (flatten (map :errors %)))]
      (handler (assoc req :uri (.replaceAll (:uri req) "\\." ":"))))))

(def main-routes (middleware crud-routes))
(intern *ns* 'defcrud #'actions/defcrud)
