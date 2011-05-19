(ns jaco.test.crud.actions
  (:use midje.sweet
        [jaco.core.actions :only [*request*]])
  (:require (jaco.crud [templates  :as tpl]
                       [routes     :as routes]
                       [datasource :as ds]))
  (:use [jaco.crud.actions :exclude [type]] :reload)
  (:import jaco.crud.actions.Field))

(facts "about props"
  (props "something wrong")
  => (throws IllegalArgumentException "There is no CRUD for the type something wrong")

  (binding [*crud-map* {:foo :bar}]
    (props :foo)) => :bar)


;;================================
;; Macro
;;================================

(defrecord Mock [a b c])
(def mock-name (.getName Mock))

;; First we'll test default actions
(defcrud Mock {:factory #(Mock. (:a %) (:b %) (:c %))}
  (:a) (:b) (:c))

(facts "about default opts"
  (let [{:keys [factory create update delete] :as opts}
        (provide-default-opts 'Mock '[(foo :something :here) (bar :and :more) (baz)])]

    opts => (contains {:title "Mock" :overview '(foo bar baz)})
    (factory anything)  => (throws UnsupportedOperationException "Factory fn is not provided")))

(facts "about default actions"
  (binding [*request* {:params {:a 1 :b 2 :c 3}}]
    (create {:* mock-name}) => (tpl/completed)
    (provided
      (props jaco.test.crud.actions.Mock) => {:factory identity}
      (ds/save! {:a 1, :b 2, :c 3}) => :ok))

  (binding [*request* {:params {:a 3 :b 2}}]
    (update {:* mock-name :id 1}) => (tpl/completed)
    (provided
      (ds/retrieve Mock 1) => {:a 1 :b 2 :c 3}
      (ds/save! {:a 3, :b 2, :c 3}) => :ok))

  (delete {:* mock-name :id 1}) => (tpl/completed)
  (provided
    (ds/retrieve Mock 1) => :entity
    (ds/delete! :entity) => :ok))


;; Now we'll test everything else, so let's redefine crud for Mock
(defn my-view [name default] (str "<input type='text' name='" name "' value='" default "' "))
(defn my-default [] "default value")
(defn my-to-string [x] (str \* x \*))

(defcrud Mock {:title "Simple mock", :overview [:a]
               :create identity, :update identity, :delete identity}
  (:a "AAA"
      :comment "Well, it's the A!"
      :view my-view
      :default my-default)
  (:b "BB"
      :to-string my-to-string)
  (:c "C"
      :default my-default))

(facts "about defcrud's generated methods"
  (binding [*request* {:* mock-name, :params "params"}]
    (create *request*) => "params"
    (update *request*) => "params"
    (delete *request*) => "params"))


(facts "about *crud-map*'s content after defcrud"
  (let [{:keys [title overview factory fields]} (*crud-map* Mock)
        {:keys [a b c]} fields]
    title => "Simple mock"
    overview => [:a]
    factory => fn?

    (:title a) => "AAA"
    (:comment a) => "Well, it's the A!"
    (:view a) => (exactly my-view)
    (:default a) => (exactly my-default)
    (:to-string a) => str

    (:title b) => "BB"
    (:comment b) => nil

    (:title c) => "C"
    (:comment c) => nil))


;;================================
;;  Actions
;;================================

(facts "about apply-to-string"
  (#'jaco.crud.actions/apply-to-string Mock (Mock. 1 2 3))
  => {:a "1" :b "*2*" :c "3"})


(facts "about create-page"
  (create-page {:* mock-name})
  => (tpl/view nil [{:title "AAA"
                     :comment "Well, it's the A!"
                     :view (my-view "a" (my-default))}
                    {:title "BB"
                     :comment nil
                     :view (tpl/default-view "b" "")}
                    {:title "C"
                     :comment nil
                     :view (tpl/default-view "c" (my-default))}]))


(facts "about update-page"
  (update-page {:* mock-name, :id "1"})
  => (tpl/view 1 [{:title "AAA"
                   :comment "Well, it's the A!"
                   :view (my-view "a" "a val")}
                  {:title "BB"
                   :comment nil
                   :view (tpl/default-view "b" "*b val*")}
                  {:title "C"
                   :comment nil
                   :view (tpl/default-view "c" "c val")}])
  (provided
    (ds/retrieve Mock 1) => (Mock. "a val" "b val" "c val")))


(facts "about overview"
  (overview {:* mock-name}) => (tpl/overview [] (routes/create mock-name))
  (provided
    (ds/retrieve-all Mock) => [])

  (overview {:* mock-name})
  => (tpl/overview [[(routes/update mock-name 1) (routes/delete mock-name 1) {:a 1}]]
                   (routes/create mock-name))
  (provided
    (ds/retrieve-all Mock) => [(Mock. 1 2 3)]
    (ds/get-id anything)   => 1))


(facts "about index"
  (index {}) => (tpl/index [{:title "Simple mock"
                             :url (routes/overview mock-name)}]))
