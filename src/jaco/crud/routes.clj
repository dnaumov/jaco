(ns jaco.crud.routes
  (:use jaco.core.routes))

(defroute create   "/:type/create")
(defroute update   "/:type/update/:id")
(defroute delete   "/:type/delete/:id")
(defroute overview "/:type/overview")
(defroute index    "/index")
