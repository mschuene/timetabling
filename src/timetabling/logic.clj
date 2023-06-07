
;; [[file:~/programming/timetabling/timetabling.org::*Representation%20von%20aussagenlogischen%20Formeln%20in%20konjunktiver%20Normalform][Representation\ von\ aussagenlogischen\ Formeln\ in\ konjunktiver\ Normalform:1]]

(ns timetabling.logic
  (:require [clojure.set :as set]))

;; Representation\ von\ aussagenlogischen\ Formeln\ in\ konjunktiver\ Normalform:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Representation%20von%20aussagenlogischen%20Formeln%20in%20konjunktiver%20Normalform][Representation\ von\ aussagenlogischen\ Formeln\ in\ konjunktiver\ Normalform:1]]

(defn gen-var
  ([x] (gen-var x true))
  ([x b]
   {:negated? (not b)
    :var x}))

;; Representation\ von\ aussagenlogischen\ Formeln\ in\ konjunktiver\ Normalform:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Representation%20von%20aussagenlogischen%20Formeln%20in%20konjunktiver%20Normalform][Representation\ von\ aussagenlogischen\ Formeln\ in\ konjunktiver\ Normalform:1]]

(defn negate [v]
  (update-in v [:negated?] not))

;; Representation\ von\ aussagenlogischen\ Formeln\ in\ konjunktiver\ Normalform:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Representation%20von%20aussagenlogischen%20Formeln%20in%20konjunktiver%20Normalform][Representation\ von\ aussagenlogischen\ Formeln\ in\ konjunktiver\ Normalform:1]]

(defn implies
  "a is a variable and b is either a single variable
   or a set of variables"
  [a b]
  (set/union #{(negate a)} (if (set? b) b #{b})))

;; Representation\ von\ aussagenlogischen\ Formeln\ in\ konjunktiver\ Normalform:1 ends here
