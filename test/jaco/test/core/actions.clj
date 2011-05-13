(ns jaco.test.core.actions
  (:use midje.sweet)
  (:use jaco.core.actions :reload-all)
  (:import jaco.core.actions.Param))

;;================================
;; Examples
;;================================

(fact "actions are fns of a map"
  ((action [] :foo) {}) => :foo)

(fact "args are vectors, first elem of which is a key"
  ((action [[:x] [:y]] (str x y)) {:x 4 :y 2}) => "42")

(fact "args evaluated, and locals named as in binding vector even if the keys are different"
  (let [a [:x], b [:y]]
    ((action [a b] (str a b)) {:x 4 :y 2}) => "42"))

(fact "rest of each arg-vector contains special functions")

(fact "such a function can be either a converter..."
  (let [conv (converter inc (constantly "error message here"))]
    ((action [[:x conv]] (str "x is " x)) {:x 1}) => "x is 2"))

(fact "or a validator. Body of the action will be executed only if validators pass."
  (def check-positive (validator pos? (constantly "not positive")))
  ((action [[:x check-positive]] x) {:x 1})
  => 1)

(fact "If not, *error-handler* is called with the errors coll as an arg"
  (binding [*error-handler* identity]
    ((action [[:x check-positive]] x) {:x 0})
    => (just [{:key :x, :val 0, :errors ["not positive"]}])))

;; return only first error message of each parameter
(alter-var-root (var *error-handler*) (constantly #(map (comp first :errors) %)))

(fact "converters fail by throwing an exception"
  (def parse-int (converter #(Integer/parseInt %) (constantly "not an int")))
  (Integer/parseInt "foo") => (throws NumberFormatException)
  ((action [[:x parse-int]] x) {:x "foo"}) => ["not an int"])

(fact "you can use :ensure? to prevent calling a fn if errors occured on the previous steps"
  (let [bad check-positive
        good (validator pos? (constantly "not pos") :ensure? true)]
    ((action [[:x parse-int bad]] x) {:x "foo"}) => (throws RuntimeException)
    ((action [[:x parse-int good]] x) {:x "foo"}) => ["not an int"]))

(fact "errfn is a fn of 3 args: key of param, its value and coll of errors"
  (let [strs {:pass "Password", :login "Login"}
        errfn (fn [k _ _] (str (k strs) " is too short!"))
        check-length (validator #(> (count %) 5) errfn)
        an-action (action [[:login check-length] [:pass check-length]])]

    (an-action {:login "pyotr" :pass "12345"})
    => ["Login is too short!" "Password is too short!"]))

(fact "*error-handler* can be rebinded for the current action with the action's opts"
  ((action {:error-handler :a-fn} [] *error-handler*) {}) => :a-fn)

(fact "body is executed even if there are errors when *error-handler* is nil"
  ((action {:error-handler nil}
     [[:x (validator (constantly false) (constantly "foo"))]]
     :body) {}) => :body)

(fact "(action ...) returns an anonymous fn, but normally you would have a named one"
  (defaction foo "doc-string"
    [[:x parse-int check-positive] [:y]]
    (str x y))
  (foo {:x "4" :y "2"}) => "42"
  (-> foo var meta :doc) => "doc-string")

(fact "you can also use keywords in args, that's the same as one-elem vector"
  ((action [:x :y :z] [z y x]) {:x 1 :y 2 :z 3}) => [3 2 1])


(comment ;; so, action's args can be reused
  (def entity [:id parse-int get-from-db ensure-non-nil])
  (defaction [entity]
    ...))


;;================================
;; Other
;;================================

(facts "about process-param"
  (process-param :foo "bar" []) => {:key :foo, :val "bar", :errors []}
  (process-param :foo "bar" [identity]) => {:key :foo, :val "bar", :errors []}

  (process-param :x 1 [(validator string? (constantly "not a str"))
                       (converter dec (constantly "dec"))
                       #(update-in % [:errors] conj "qux")])
  => {:key :x, :val 0, :errors ["not a str" "qux"]})


(facts "about process-all-params"
  (process-all-params {:x 10 :y 20} [[:x] [:y]])
  => {:x (Param. :x 10 []), :y (Param. :y 20 [])}

  (process-all-params {:x 10 :y 20} [[:x #(update-in % [:errors] conj "foo")]
                                     [:y #(assoc % :val "bar")]])
  => {:x (Param. :x 10 ["foo"]), :y (Param. :y "bar" [])})


(facts "about validators and converters"
  (let [app (fn [value & fs] (reduce #(%2 %1) (Param. :x value []) fs))
        parse-int (converter #(Integer/parseInt %) (constantly "NaN"))
        check-pos  (validator pos? (constantly "number must be positive") :ensure? true)
        increment (converter inc (constantly "integer overflow, i think") :ensure? true)]

    (app "1" parse-int) => (Param. :x 1 [])
    (app "O" parse-int) => (Param. :x "O" ["NaN"])

    (app 1 check-pos) => (Param. :x 1 [])
    (app 0 check-pos) => (Param. :x 0 ["number must be positive"])

    (app "1" parse-int check-pos) => (Param. :x 1 [])
    (app "O" parse-int check-pos) => (Param. :x "O" ["NaN"])

    (app "1" parse-int check-pos increment) => (Param. :x 2 [])
    (app "0" parse-int check-pos increment) => (Param. :x 0 ["number must be positive"])))
