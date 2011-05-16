(ns jaco.crud.routes
  (:use jaco.core.routes))

(defroute create   "/*/create")
(defroute update   "/*/update/:id")
(defroute delete   "/*/delete/:id")
(defroute overview "/*/overview")
(defroute index    "/")
