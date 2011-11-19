(ns jaco.test.core.actions
  (:use midje.sweet)
  (:use jaco.core.actions :reload-all))

;; actions will return :error if validator/converter has failed (more on that later)
(alter-var-root #'*error-handler* (constantly (constantly :error)))


(fact "Actions are functions of one argument - a map."
  ((action [] :foo) {}) => :foo)

(fact "The map is automatically destructed (as in {:keys [...]})."
  ((action [x y] (str x y)) {:x 4 :y 2}) => "42")

(fact "You can specify validator for a param. Body of the action
        will be executed only if validator returned true."
  (let [a (action [:pos? x] x)]
    (a {:x 1}) => 1
    (a {:x 0}) => :error))

(fact "Also, you can specify a converter - fn which will be applied to the param's value."
  (let [a (action [:int x] x)]
    (a {:x "1"}) => 1
    (a {:x "not-convertable-to-int"}) => :error))

(fact "It's possible to specify more that one validator/converter."
  ((action [:int :pos? :inc x] x) {:x "1"}) => 2)

(fact "To pass arguments to a converter or validator, use the following syntax:"
  ((action [(:max-length 10) x] x) {:x "foo"}) => "foo")

(fact "When you want to specify several validators with a single arg, you
       can use alternative syntax with maps. It's a little more concise,
       but remember that order of map entries doesn't guaranteed to preserve."
  (let [a (action [{:min-length 3 :max-length 6} x] x)]
    (a {:x "foo"}) => "foo"
    (a {:x "f"}) => :error))

(fact "If some combination of validators/converters is used often, then it
       makes sense to save it in the var and reuse it in several actions."
  (let [password [[:min-length 6] [:max-length 16] [:make-hash]]]
    ((action [^password x] x) {:x "hacker"}) => (hash "hacker")))

(fact "All of the above options can be used together."
  (let [a (action [{:min-length 2 :max-length 3} (:matches? #"[0-9]+") :int x] x)]
    (a {:x "12"}) => 12
    (a {:x "foo"}) => :error))

(fact "You can define your own converters and validators. "
  (defconverter :adder [x] (fn [val] (+ val x)))
  ((action [{:adder 10} x] x) {:x 1}) => 11

  (defvalidator :funky? [_] #{"the meters" "dirty dozen brass band"})

  (let [f (comp :passed? (make-param-fn :funky? nil))]
    (f "the meters") => true
    (f "metallica") => false))

;; TODO: error-handler stuff

(fact "*error-handler* can be rebound for the current action."
  ((action {:error-handler :a-fn} [] *error-handler*) {}) => :a-fn)

(fact "When *error-handler* is nil, body of the action is executed even if there are errors."
  ((action {:error-handler nil} [:pos? x] :body)
   {:x -1}) => :body)


(tabular
 (fact (transform-params-list (quote ?from)) => (quote ?to))
 ?from                          ?to
 [x y]                          {x [] y []}
 [:a :b x]                      {x [[:a] [:b]]}
 [:a x y]                       {x [[:a]] y []}
 [(:a 1) x]                     {x [[:a 1]]}
 [{:a 1 :b 2} x, {:a 3 :b 4} y] {x [[:a 1] [:b 2]] y [[:a 3] [:b 4]]}
 [:a {:b 1} (:c 2) x]           {x [[:a] [:b 1] [:c 2]]}
 [^foo x]                       {x foo})


(tabular
 (fact (process-param 'x ?value ?checkers) => ['x ?new-value ?failed-checkers])
 ?value  ?checkers                 ?new-value ?failed-checkers
 "42"    [[:max-length 3] [:int]]  42         []
 "42"    [[:int] [:inc]]           43         []
 "foooo" [[:max-length 3]]         "foooo"    [[:max-length 3]]
 "qwe"   [[:int]]                  "qwe"      [[:int]]
 "qwer"  [[:int] [:max-length 3]]  "qwer"     [[:int] [:max-length 3]]
 (against-background
   (make-param-fn :max-length 3) => (validator #(<= (count %) 3))
   (make-param-fn :int) => (converter #(Integer/parseInt %))
   (make-param-fn :inc) => (converter inc)))
