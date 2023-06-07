
;; [[file:~/programming/timetabling/timetabling.org::*Einf%C3%BChrung][Einführung:1]]

(ns timetabling.core
  (:require [timetabling.cardinality :refer :all]
            [timetabling.logic :refer :all]
            [clojure.test :refer :all]
            [clojure.set :as set]
            [clojure.java.shell :as sh]
            [seesaw.core :refer :all]))

;; Einführung:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Utilities%20for%20encoding][Utilities\ for\ encoding:1]]

(def entities
  {:classes []
   :events []
   :rooms []
   :buildings []
   :teachers []
   :num-days 5
   :distances {}
   :hours-per-day 6})

(defn create-event [& {:keys [classes rooms teachers durations times number-of-times forbidden-rooms]}]
  {:classes (if (seq? classes) classes #{classes})
   :rooms (if rooms (if (and (set? rooms) number-of-times) (vec (repeat number-of-times rooms)) rooms ) (vec (repeat number-of-times nil)))
   :teachers (if (and (set? teachers) number-of-times) (vec (repeat number-of-times teachers)) teachers)
   :durations (if (and (not durations) number-of-times) (vec (repeat number-of-times 1))  durations)
   :forbidden-rooms (or forbidden-rooms #{})
   :times (or times (vec (repeat number-of-times nil)))
   })


(defn events-to-schedule
  "unfolds events that are to be held multiple times a week"
  [events]
  (for [[n details] events
        idx (range (count (:durations details)))]
    [(keyword (str (name n) ":" idx))
     {:classes (:classes details) :rooms (nth (:rooms details) idx)
      :teachers (nth (:teachers details) idx)
      :duration (nth (:durations details) idx)
      :forbidden-rooms (:forbidden-rooms details)
      :time (nth (:times details) idx)}]))


(defn next-times-at-day
  "returns the num next [day hour] pairs at that day, possibly less when 
   hours-per-day is reached"
  [[day hour] hours-per-day num]
  (map vector (repeat day) (range (inc hour) (min hours-per-day (+ hour num 1)))))

(defn previous-times-at-day
 [[day hour] num]
 (map vector (repeat day) (range (dec hour) (max -1 (- hour num 1)) -1)))

(deftest test-next-times-at-day
  (is (= '([2 3] [2 4] [2 5]) (next-times-at-day [2 2] 6 4)))
  (is (= '([2 3]) (next-times-at-day [2 2] 6 1))))

(test-next-times-at-day)


(deftest test-previous-times-at-day
  (is (= '([2 2] [2 1])
         (previous-times-at-day [2 3] 4))))

(test-previous-times-at-day)

;; Utilities\ for\ encoding:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

(def example-entities {

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:classes {:class-1 {:class-teacher :teacher-1 :class-room [:building-1 :room-1]}
          :class-2 {:class-teacher :teacher-2 :class-room [:building-2 :room-1]}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:events {

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-math
(create-event :classes :class-1 :rooms #{[:building-1 :room-1]} :teachers #{:teacher-1} :number-of-times 4) ;;hier auf 30 ändern
:class-2-math
(create-event :classes :class-2 :rooms #{[:building-2 :room-1]} :teachers #{:teacher-2} :number-of-times 4)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-sk 
(create-event :classes :class-1 :rooms #{[:building-1 :room-1]} :teachers #{:teacher-1} :number-of-times 4)
:class-2-sk
(create-event :classes :class-2 :rooms #{[:building-2 :room-1]} :teachers #{:teacher-2} :number-of-times 4)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-german
(create-event :classes :class-1 :rooms #{[:building-1 :room-1]} :teachers #{:teacher-1} :number-of-times 4)         
:class-2-german
(create-event :classes :class-2 :rooms #{[:building-2 :room-1]} :teachers #{:teacher-2} :number-of-times 4)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-sport
(create-event :classes :class-1 :rooms #{[:sporthalle :sporthalle]} :teachers #{:teacher-2} :durations [2] :number-of-times 1)         
:class-2-sport
(create-event :classes :class-2 :rooms #{[:sporthalle :sporthalle]} :teachers #{:teacher-2} :durations [2] :number-of-times 1)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-art
(create-event :classes :class-1 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-1} :number-of-times 4)         
:class-2-art
(create-event :classes :class-2 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-1} :number-of-times 4)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-werken
(create-event :classes :class-1 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-1} :number-of-times 2)        
:class-2-werken
(create-event :classes :class-2 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-1} :number-of-times 2)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:buildings {

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:building-1 {:room-1 {} :art-room {}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:building-2 {:room-1 {} :werkraum {}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:sporthalle {:sporthalle
             {:times-not-available
              (set/difference (into #{} (for [d (range 5) h (range 5)] [d h]))
                              #{[3 1] [3 2] [3 3] [3 4]})}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:num-days 5
:hours-per-day 6

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:teachers {:teacher-1 {} :teacher-2 {}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:distances {[:building-1 :building-2] 1
            [:building-2 :building-1] 1
            [:building-1 :sporthalle] 1
            [:sporthalle :building-1] 1
            [:building-2 :sporthalle] 1
            [:sporthalle :building-2] 1}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

})

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

(def bigger-example-entities {

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:classes {:class-1 {:class-teacher :teacher-1 :class-room [:building-1 :room-1]}
          :class-2 {:class-teacher :teacher-2 :class-room [:building-2 :room-1]}
          :class-3 {:class-teacher :teacher-3 :class-room [:building-1 :room-2]}
          :class-4 {:class-teacher :teacher-4 :class-room [:building-2 :room-2]}
          :class-5 {:class-teacher :teacher-5 :class-room [:building-1 :room-3]}
          :class-6 {:class-teacher :teacher-6 :class-room [:building-2 :room-3]}
          :class-7 {:class-teacher :teacher-7 :class-room [:building-1 :room-4]}
          :class-8 {:class-teacher :teacher-8 :class-room [:building-2 :room-4]}
          :class-9 {:class-teacher :teacher-9 :class-room [:building-1 :room-5]}
          :class-10 {:class-teacher :teacher-10 :class-room [:building-2 :room-5]}
          :class-11 {:class-teacher :teacher-11 :class-room [:building-1 :room-6]}
          :class-12 {:class-teacher :teacher-12 :class-room [:building-2 :room-6]}
          }

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:events {

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-math
(create-event :classes :class-1 :rooms #{[:building-1 :room-1]} :teachers #{:teacher-1} :number-of-times 4) ;;hier auf 30 ändern
:class-2-math
(create-event :classes :class-2 :rooms #{[:building-2 :room-1]} :teachers #{:teacher-2} :number-of-times 4)
:class-3-math
(create-event :classes :class-3 :rooms #{[:building-1 :room-2]} :teachers #{:teacher-3} :number-of-times 4) ;;hier auf 30 ändern
:class-4-math
(create-event :classes :class-4 :rooms #{[:building-2 :room-2]} :teachers #{:teacher-4} :number-of-times 4)
:class-5-math
(create-event :classes :class-5 :rooms #{[:building-1 :room-3]} :teachers #{:teacher-5} :number-of-times 4) ;;hier auf 30 ändern
:class-6-math
(create-event :classes :class-6 :rooms #{[:building-2 :room-3]} :teachers #{:teacher-6} :number-of-times 4):class-7-math
(create-event :classes :class-7 :rooms #{[:building-1 :room-4]} :teachers #{:teacher-7} :number-of-times 4) ;;hier auf 30 ändern
:class-8-math
(create-event :classes :class-8 :rooms #{[:building-2 :room-4]} :teachers #{:teacher-8} :number-of-times 4)
:class-9-math
(create-event :classes :class-9 :rooms #{[:building-1 :room-5]} :teachers #{:teacher-9} :number-of-times 4) ;;hier auf 30 ändern
:class-10-math
(create-event :classes :class-10 :rooms #{[:building-2 :room-5]} :teachers #{:teacher-10} :number-of-times 4):class-11-math
(create-event :classes :class-11 :rooms #{[:building-1 :room-6]} :teachers #{:teacher-11} :number-of-times 4) ;;hier auf 30 ändern
:class-12-math
(create-event :classes :class-12 :rooms #{[:building-2 :room-6]} :teachers #{:teacher-12} :number-of-times 4)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-sk 
(create-event :classes :class-1 :rooms #{[:building-1 :room-1]} :teachers #{:teacher-1} :number-of-times 4)
:class-2-sk
(create-event :classes :class-2 :rooms #{[:building-2 :room-1]} :teachers #{:teacher-2} :number-of-times 4)
:class-3-sk 
(create-event :classes :class-3 :rooms #{[:building-1 :room-2]} :teachers #{:teacher-3} :number-of-times 4)
:class-4-sk
(create-event :classes :class-4 :rooms #{[:building-2 :room-2]} :teachers #{:teacher-4} :number-of-times 4)
:class-5-sk
(create-event :classes :class-5 :rooms #{[:building-1 :room-3]} :teachers #{:teacher-5} :number-of-times 4)
:class-6-sk
(create-event :classes :class-6 :rooms #{[:building-2 :room-3]} :teachers #{:teacher-6} :number-of-times 4)
:class-7-sk 
(create-event :classes :class-7 :rooms #{[:building-1 :room-4]} :teachers #{:teacher-7} :number-of-times 4)
:class-8-sk
(create-event :classes :class-8 :rooms #{[:building-2 :room-4]} :teachers #{:teacher-8} :number-of-times 4)
:class-9-sk 
(create-event :classes :class-9 :rooms #{[:building-1 :room-5]} :teachers #{:teacher-9} :number-of-times 4)
:class-10-sk
(create-event :classes :class-10 :rooms #{[:building-2 :room-5]} :teachers #{:teacher-10} :number-of-times 4)
:class-11-sk 
(create-event :classes :class-11 :rooms #{[:building-1 :room-6]} :teachers #{:teacher-11} :number-of-times 4)
:class-12-sk
(create-event :classes :class-12 :rooms #{[:building-2 :room-6]} :teachers #{:teacher-12} :number-of-times 4)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-german
(create-event :classes :class-1 :rooms #{[:building-1 :room-1]} :teachers #{:teacher-1} :number-of-times 4)         
:class-2-german
(create-event :classes :class-2 :rooms #{[:building-2 :room-1]} :teachers #{:teacher-2} :number-of-times 4)         
:class-3-german
(create-event :classes :class-3 :rooms #{[:building-1 :room-2]} :teachers #{:teacher-3} :number-of-times 4)         
:class-4-german
(create-event :classes :class-4 :rooms #{[:building-2 :room-2]} :teachers #{:teacher-4} :number-of-times 4)         
:class-5-german
(create-event :classes :class-5 :rooms #{[:building-1 :room-3]} :teachers #{:teacher-5} :number-of-times 4)         
:class-6-german
(create-event :classes :class-6 :rooms #{[:building-2 :room-3]} :teachers #{:teacher-6} :number-of-times 4)         
:class-7-german
(create-event :classes :class-7 :rooms #{[:building-1 :room-4]} :teachers #{:teacher-7} :number-of-times 4)         
:class-8-german
(create-event :classes :class-8 :rooms #{[:building-2 :room-4]} :teachers #{:teacher-8} :number-of-times 4)         
:class-9-german
(create-event :classes :class-9 :rooms #{[:building-1 :room-5]} :teachers #{:teacher-9} :number-of-times 4)         
:class-10-german
(create-event :classes :class-10 :rooms #{[:building-2 :room-5]} :teachers #{:teacher-10} :number-of-times 4)         
:class-11-german
(create-event :classes :class-11 :rooms #{[:building-1 :room-6]} :teachers #{:teacher-11} :number-of-times 4)         
:class-12-german
(create-event :classes :class-12 :rooms #{[:building-2 :room-6]} :teachers #{:teacher-12} :number-of-times 4)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-sport
(create-event :classes :class-1 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-2-sport
(create-event :classes :class-2 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-3-sport
(create-event :classes :class-3 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-4-sport
(create-event :classes :class-4 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-5-sport
(create-event :classes :class-5 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-6-sport
(create-event :classes :class-6 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-7-sport
(create-event :classes :class-7 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-8-sport
(create-event :classes :class-8 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-9-sport
(create-event :classes :class-9 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-10-sport
(create-event :classes :class-10 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-11-sport
(create-event :classes :class-11 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
:class-12-sport
(create-event :classes :class-12 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-art
(create-event :classes :class-1 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-1} :number-of-times 2)         
:class-2-art
(create-event :classes :class-2 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-2} :number-of-times 2)         
:class-3-art
(create-event :classes :class-3 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-3} :number-of-times 2)         
:class-4-art
(create-event :classes :class-4 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-4} :number-of-times 2)         
:class-5-art
(create-event :classes :class-5 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-5} :number-of-times 2)         
:class-6-art
(create-event :classes :class-6 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-6} :number-of-times 2)         
:class-7-art
(create-event :classes :class-7 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-7} :number-of-times 2)         
:class-8-art
(create-event :classes :class-8 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-8} :number-of-times 2)         
:class-9-art
(create-event :classes :class-9 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-9} :number-of-times 2)         
:class-10-art
(create-event :classes :class-10 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-10} :number-of-times 2)         
:class-11-art
(create-event :classes :class-11 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-11} :number-of-times 2)         
:class-12-art
(create-event :classes :class-12 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-12} :number-of-times 2)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:class-1-werken
(create-event :classes :class-1 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-1} :number-of-times 2)        
:class-2-werken
(create-event :classes :class-2 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-2} :number-of-times 2)         
:class-3-werken
(create-event :classes :class-3 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-3} :number-of-times 2)        
:class-4-werken
(create-event :classes :class-4 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-4} :number-of-times 2)         
:class-5-werken
(create-event :classes :class-5 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-5} :number-of-times 2)        
:class-6-werken
(create-event :classes :class-6 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-6} :number-of-times 2)         
:class-7-werken
(create-event :classes :class-7 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-7} :number-of-times 2)        
:class-8-werken
(create-event :classes :class-8 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-8} :number-of-times 2)         
:class-9-werken
(create-event :classes :class-9 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-9} :number-of-times 2)        
:class-10-werken
(create-event :classes :class-10 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-10} :number-of-times 2)         
:class-11-werken
(create-event :classes :class-11 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-11} :number-of-times 2)        
:class-12-werken
(create-event :classes :class-12 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-12} :number-of-times 2)

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:buildings {

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:building-1 {:room-1 {}
             :room-2 {}
             :room-3 {}
             :room-4 {}
             :room-5 {}
             :room-6 {}
             :art-room {}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:building-2 {:room-1 {}
             :room-2 {}
             :room-3 {}
             :room-4 {}
             :room-5 {}
             :room-6 {}
             :werkraum {}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:sporthalle {:sporthalle
             {}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:num-days 5
:hours-per-day 6

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:teachers {:teacher-1 {}
           :teacher-2 {}
           :teacher-3 {}
           :teacher-4 {}
           :teacher-5 {}
           :teacher-6 {}
           :teacher-7 {}
           :teacher-8 {}
           :teacher-9 {}
           :teacher-10 {}
           :teacher-11 {}
           :teacher-12 {}
           :sportlehrer {}}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

:distances {[:building-1 :building-2] 1
                       [:building-2 :building-1] 1
                       [:building-1 :sporthalle] 1
                       [:sporthalle :building-1] 1
                       [:building-2 :sporthalle] 1
                       [:sporthalle :building-2] 1}

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einfaches%20Beispielproblem][Einfaches\ Beispielproblem:1]]

})

;; Einfaches\ Beispielproblem:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*erzeugen%20einer%20dimacs%20Datei%20aus%20clauseln][erzeugen\ einer\ dimacs\ Datei\ aus\ clauseln:1]]

(defn encode-literal [name-map literal mkey]
  (if-let [numb (get name-map (:var literal))]
    [name-map (if (:negated? literal) (- numb) numb) mkey]
    (recur (assoc name-map (:var literal) (inc mkey)) literal (inc mkey))))

(defn encode-clause [name-map clause mk]
  (reduce (fn [[name-map partial-clause mkey] next-literal]
            (let [[name-map encoded-literal max-key]
                  (encode-literal name-map next-literal mkey)]
              [name-map (conj partial-clause encoded-literal) max-key]))
          [name-map [] mk]
          clause))

;;neues Format für Clauses:
;;clauses = clause +
;;clause = #{literal+} | [ #{literal+} weigth]

(defn encode
  "encodes the clauses to dimacs format"
  ([clauses] (encode clauses true))
  ([clauses max-sat?]
   (let [[name-map encoded mkey weights]
         (reduce (fn [[name-map partial-clauses mkey ws] next-clause]
                   (let [soft-clause? (and max-sat? (sequential? next-clause))
                         [name-map encoded-clause max-key]
                         (encode-clause
                          name-map (if soft-clause?
                                     (first next-clause)
                                     next-clause) mkey)]
                     [name-map (conj partial-clauses encoded-clause) max-key
                      (conj ws (if soft-clause? (second next-clause) :hard))]))
                 [{} [] 0 []]
                 clauses)
         hard-weigth (max 1 (reduce + (remove #{:hard} weights)))]
     [name-map
      (apply str "c generated " (if max-sat? "wcnf" "cnf") " file\n"
             "p " (if max-sat? "wcnf " "cnf ")
             (count (keys name-map)) " "
             (count encoded)
             (if max-sat? (str " " hard-weigth) "") "\n"
             (interpose
              "\n"
              (map #(apply str
                           (if max-sat?
                             (str (if (= :hard %2) hard-weigth %2) " ")
                             "")
                           (concat (interpose " " %1) " 0"))
                   encoded weights)))])))

(defn parse-output [output name-map]
  (let [lines (.split output "\n")
        inv-map (->> (for [[a b] name-map] [b a]) (into {}))]
    (reduce
     (fn [var-map line]
       (if (.startsWith line "v ")
         (merge var-map
                (->> (rest (.split line " "))
                     (map #(Integer/parseInt %))
                     (remove #{0}) ;;s4j includes a 0 in the end
                     (map (fn [v] 
                            (let [inv (inv-map (Math/abs v))]
                              [inv (pos? v)])))
                     (into {})))
         (do (when (.startsWith line "o ")
               (prn "best weight so far " line))
             var-map))) nil lines)))

(def last-name-map (atom nil))

(defn get-assignment
  ([clauses] (get-assignment clauses "resources/cnf1" true))
  ([clauses filename] (get-assignment clauses filename true))
  ([clauses filename maxsat?]
   (let [[name-map encoded] (encode clauses maxsat?)
         _ (spit filename encoded)
         _ (reset! last-name-map name-map)
         _ (spit "resources/last-name-map.edn" (pr-str name-map))
         _ (prn "starting solver")
         output
         (time (:out
          (sh/sh "sh" "-c"
                 (if maxsat?
                 "timeout -s SIGTERM 300 ./resources/open-wbo_static resources/cnf1 > resources/output"
                   "timeout -s SIGTERM 1800 java -jar resources/sat4j-sat.jar resources/cnf1 > resources/output")))) 
         output (slurp "resources/output")]
     (spit "resources/current-output" output)
     (println "sat solver finished")
     (parse-output output name-map))))



(deftest test-get-assignment
  (is (= '{x true, y false}
         (get-assignment [#{(gen-var 'x)} #{(gen-var 'y false)}])))
  (is (nil? (get-assignment [#{(gen-var 'x false)} #{ (gen-var 'x)}])))
  (is (= (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (concat (at-most (map (comp gen-var #(str "a" %)) (range 10)) 1) (at-least (map (comp gen-var #(str "a" %)) (range 10)) 1))))) 1))
  (is (= 0 (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (concat (at-most (map (comp gen-var #(str "a" %)) (range 9)) 4) (map (comp (fn [s] #{s}) gen-var #(str "a" %)) (range 5))))))))
  (is (= 15 (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (exactly (map (comp gen-var #(str "a" %)) (range 20)) 15))))))
  (is (= 1 (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (concat (exactly (map (comp gen-var #(str "a" %)) (range 20)) 1) (map (comp (fn [s] #{s}) negate gen-var #(str "a" %)) (range 19))))))))
  (is (= 0 (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (concat (exactly (map (comp gen-var #(str "a" %)) (range 20)) 1) (map (comp (fn [s] #{s}) negate gen-var #(str "a" %)) (range 20))))))))
  (is (= 0 (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (concat (exactly (map (comp gen-var #(str "a" %)) (range 20)) 1) (map (comp (fn [s] #{s}) gen-var #(str "a" %)) (range 2))))))))
  (is (= 1 (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (concat (exactly (map (comp gen-var #(str "a" %)) (range 20)) 1) (map (comp (fn [s] #{s}) gen-var #(str "a" %)) (range 1))))))))
  (is (= 0 (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (concat (at-most (map (comp gen-var #(str "a" %)) (range 9)) 4) (map (comp (fn [s] #{s}) gen-var #(str "a" %)) (range 5))))))))
  (is (= 4 (count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (concat (at-most (map (comp gen-var #(str "a" %)) (range 9)) 4) (map (comp (fn [s] #{s}) gen-var #(str "a" %)) (range 4)))))))))


(test-get-assignment)


(defn complete-entities  [entities assignment]
  (reduce-kv (fn [entities k v]
               (cond
                 (and v (.startsWith (name k) "starts-at-hour"))
                 (let [[_ event-id day hour] (.split (name k) "_")
                       [event-name occ] (.split event-id ":")]
                   (assoc-in entities [:events
                                       (keyword event-name)
                                       :times
                                       (Integer/parseInt occ)]
                                       [(Integer/parseInt day) (Integer/parseInt hour)]))
                 (and v (.startsWith (name k) "occurs-at-room"))
                 (let [[_ event-id building-name room-name] (.split (name k) "_")
                       [event-name occ] (.split event-id ":")]
                   (assoc-in entities [:events
                                       (keyword event-name)
                                       :rooms
                                       (Integer/parseInt occ)]
                             #{[(keyword building-name) (keyword room-name)]}))
                 :else entities)) entities assignment))

(defn fill-timetable [events-for-class num-days hours-per-day]
  (let [timetable (vec (repeat hours-per-day
                               (vec (repeat num-days ""))))]
    (reduce (fn [timetable [event-name details]]
              (update-in timetable (vec (reverse (:time details)))
                             #(str (if (seq %) (str "Doppelbelegung " (apply str (take 20 %))) "")
                                   event-name  " in " (first (:rooms details)) " " details))) timetable events-for-class)))

(defn get-solution [entities]
  (complete-entities entities (parse-output (slurp "resources/output") @last-name-map)))

(defn show-timetable [title timetable]
  (-> (frame :title title
             :content (scrollable (table :model [:columns
                                                 (mapv (fn [d] {:key (keyword (str "day-" d))
                                                                :text (str d)})
                                                       (range (count (first timetable))))
                                                 :rows timetable])))
      pack! show!))

(defmulti show-timetable-for (fn [type name entities] type))

(defmethod show-timetable-for :class [_ class-name {:keys [events num-days hours-per-day]}]
  (let [events-for-class
        (for [[event {classes :classes :as details}] (events-to-schedule events)
              :when (classes class-name)]
          [event details])]
    (show-timetable (str "timetable-for-class " class-name)
                    (fill-timetable events-for-class num-days hours-per-day))))

(defmethod show-timetable-for :teacher [_ teacher-name {:keys [events num-days hours-per-day]}]
  (let [events-for-teacher
        (for [[event {teachers :teachers :as details}] (events-to-schedule events)
              :when (teachers teacher-name)]
          [event details])]
    (show-timetable (str "timetable-for-teacher " teacher-name)
                    (fill-timetable events-for-teacher num-days hours-per-day))))

(defmethod show-timetable-for :room [_ room-name {:keys [events num-days hours-per-day]}]
  (let [events-for-room
        (for [[event {rooms :rooms :as details}] (events-to-schedule events)
              :when (rooms room-name)]
          [event details])]
    (show-timetable (str "timetable-for-room " room-name)
                    (fill-timetable events-for-room num-days hours-per-day))))

;; erzeugen\ einer\ dimacs\ Datei\ aus\ clauseln:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einf%C3%BChrung%20von%20Variablen][Einführung\ von\ Variablen:1]]

(defn occurs-at-hour
  "event is the name of the event, hour is an integer"
  [event [day hour]]
  (gen-var (symbol (str "occurs-at-hour_" (name event) "_" day "_" hour))))

;; Einführung\ von\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einf%C3%BChrung%20von%20Variablen][Einführung\ von\ Variablen:1]]

(defn occurs-at-day
    "event is the name of the event, day is an integer"
    [event day]
    (gen-var (symbol (str "occurs-at-day_" (name event) "_" day))))

;; Einführung\ von\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einf%C3%BChrung%20von%20Variablen][Einführung\ von\ Variablen:1]]

(defn occurs-at-room 
  "event is the name of the event, day is an integer"
  [event [building room]]
  (gen-var (symbol (str "occurs-at-room_" (name event) "_" (name building) "_" (name room)))))

;; Einführung\ von\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einf%C3%BChrung%20von%20Variablen][Einführung\ von\ Variablen:1]]

(defn teached-at [class [day hour]]
  (gen-var (symbol (str "teached-at_" (name class) "_" day "_" hour))))

;; Einführung\ von\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Einf%C3%BChrung%20von%20Variablen][Einführung\ von\ Variablen:1]]

(defn teaches-at [teacher [day hour]]
  (gen-var (symbol (str "teaches-at_" (name teacher) "_" day "_" hour))))

;; Einführung\ von\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beziehungen%20zwischen%20den%20Variablen][Beziehungen\ zwischen\ den\ Variablen:1]]

(defn occurs-at-day-occurs-at-hour-relations
  [{:keys [events num-days hours-per-day]}]
  (concat
   (for [[event _] (events-to-schedule events)
         day (range 0 num-days)
         hour (range 0 hours-per-day)]
     (implies (occurs-at-hour event [day hour])
              (occurs-at-day event day)))
   (for [[event _] events
         day (range 0 num-days)
         :let [occurs-at-hours
               (for [hour (range 0 hours-per-day)]
                 (occurs-at-hour event [day hour]))]]
     (implies (occurs-at-day event day) (into #{} occurs-at-hours)))))

;; Beziehungen\ zwischen\ den\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beziehungen%20zwischen%20den%20Variablen][Beziehungen\ zwischen\ den\ Variablen:1]]

(deftest day-hour-relations
  (is (= '(#{{:negated? true, :var occurs-at-hour_a:0_0_0}
   {:negated? false, :var occurs-at-day_a:0_0}}
 #{{:negated? false, :var occurs-at-day_a:0_0}
   {:negated? true, :var occurs-at-hour_a:0_0_1}}
 #{{:negated? false, :var occurs-at-day_a:0_1}
   {:negated? true, :var occurs-at-hour_a:0_1_0}}
 #{{:negated? true, :var occurs-at-hour_a:0_1_1}
   {:negated? false, :var occurs-at-day_a:0_1}}
 #{{:negated? false, :var occurs-at-hour_a_0_0}
   {:negated? true, :var occurs-at-day_a_0}
   {:negated? false, :var occurs-at-hour_a_0_1}}
 #{{:negated? false, :var occurs-at-hour_a_1_0}
   {:negated? false, :var occurs-at-hour_a_1_1}
   {:negated? true, :var occurs-at-day_a_1}})
          (occurs-at-day-occurs-at-hour-relations {:events {:a {:durations [1]}} :num-days 2 :hours-per-day 2}))))

(day-hour-relations)

;; Beziehungen\ zwischen\ den\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beziehungen%20zwischen%20den%20Variablen][Beziehungen\ zwischen\ den\ Variablen:1]]

(defn occurs-at-hour-teached-at-relations
  [{:keys [events num-days hours-per-day classes]}]
  (concat
   (for [[event {classes-for-event :classes}] (events-to-schedule events)
         day (range num-days)
         hour (range hours-per-day)
         class classes-for-event]
     (implies (occurs-at-hour event [day hour])
              (teached-at class [day hour])))
   (for [[class _] classes
         :let [events-for-class (for [[event {cl :classes}]
                                      (events-to-schedule events)
                                      :when (cl class)]
                                  event)]
         day (range num-days)
         hour (range hours-per-day)]
     (implies (teached-at class [day hour])
              (into #{} (map #(occurs-at-hour % [day hour])
                             events-for-class))))))

;; Beziehungen\ zwischen\ den\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beziehungen%20zwischen%20den%20Variablen][Beziehungen\ zwischen\ den\ Variablen:1]]

(deftest test-occurs-at-teached-at-relations
  (is (= '(#{{:negated? true, :var occurs-at-hour_b:0_0_0} {:negated? false, :var teached-at_class1_0_0}} #{{:negated? false, :var teached-at_class1_0_1} {:negated? true, :var occurs-at-hour_b:0_0_1}} #{{:negated? true, :var occurs-at-hour_b:0_1_0} {:negated? false, :var teached-at_class1_1_0}} #{{:negated? false, :var teached-at_class1_1_1} {:negated? true, :var occurs-at-hour_b:0_1_1}} #{{:negated? true, :var occurs-at-hour_a:0_0_0} {:negated? false, :var teached-at_class1_0_0}} #{{:negated? true, :var occurs-at-hour_a:0_0_0} {:negated? false, :var teached-at_class2_0_0}} #{{:negated? true, :var occurs-at-hour_a:0_0_1} {:negated? false, :var teached-at_class1_0_1}} #{{:negated? false, :var teached-at_class2_0_1} {:negated? true, :var occurs-at-hour_a:0_0_1}} #{{:negated? false, :var teached-at_class1_1_0} {:negated? true, :var ccurs-at-hour_a:0_1_0}} #{{:negated? true, :var occurs-at-hour_a:0_1_0} {:negated? false, :var teached-at_class2_1_0}} #{{:negated? false, :var teached-at_class1_1_1} {:negated? true, :var occurs-at-hour_a:0_1_1}} #{{:negated? true, :var occurs-at-hour_a:0_1_1} {:negated? false, :var teached-at_class2_1_1}} #{{:negated? false, :var occurs-at-hour_a:0_0_0} {:negated? false, :var occurs-at-hour_b:0_0_0} {:negated? true, :var teached-at_class1_0_0}} #{{:negated? true, :var teached-at_class1_0_1} {:negated? false, :var occurs-at-hour_a:0_0_1} {:negated? false, :var occurs-at-hour_b:0_0_1}} #{{:negated? true, :var teached-at_class1_1_0} {:negated? false, :var occurs-at-hour_a:0_1_0} {:negated? false, :var occurs-at-hour_b:0_1_0}} #{{:negated? false, :var occurs-at-hour_b:0_1_1} {:negated? false, :var occurs-at-hour_a:0_1_1} {:negated? true, :var teached-at_class1_1_1}} #{{:negated? false, :var occurs-at-hour_a:0_0_0} {:negated? true, :var teached-at_class2_0_0}} #{{:negated? false, :var occurs-at-hour_a:0_0_1} {:negated? true, :var teached-at_class2_0_1}} #{{:negated? false, :var occurs-at-hour_a:0_1_0} {:negated? true, :var teached-at_class2_1_0}} #{{:negated? true, :var teached-at_class2_1_1} {:negated? false, :var occurs-at-hour_a:0_1_1}})
         (occurs-at-hour-teached-at-relations {:events {:a {:classes #{:class1 :class2} :durations [1]} :b {:classes #{:class1} :durations [1]}} :num-days 2 :hours-per-day 2 :classes #{:class1 :class2}}))))


(test-occurs-at-teached-at-relations)

;; Beziehungen\ zwischen\ den\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beziehungen%20zwischen%20den%20Variablen][Beziehungen\ zwischen\ den\ Variablen:1]]

(defn occurs-at-hour-teaches-at-relations
  [{:keys [events num-days hours-per-day teachers]}]
  (concat
   (for [[event {teachers-for-event :teachers}]
         (events-to-schedule events)
         day (range num-days)
         hour (range hours-per-day)
         teacher teachers-for-event]
     (implies (occurs-at-hour event [day hour])
              (teaches-at teacher [day hour])))
   (for [[teacher _] teachers
         :let [events-for-teacher (for [[event {tchs :teachers}]
                                        (events-to-schedule events)
                                        :when (tchs teacher)]
                                    event)]
         day (range num-days)
         hour (range hours-per-day)]
     (implies (teaches-at teacher [day hour])
              (into #{} (map #(occurs-at-hour % [day hour])
                             events-for-teacher))))))

;; Beziehungen\ zwischen\ den\ Variablen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Verhindern%20von%20Doppelbelegungen][Verhindern\ von\ Doppelbelegungen:1]]

(defn class-clash-constraints
  [{:keys [classes events num-days hours-per-day]}]
  (for [[class _] classes
        :let [events-for-class
              (for [[e d] (events-to-schedule events)
                    :when ((:classes d) class)]
                e)]
        :when (seq events-for-class)
        day (range num-days)
        hour (range hours-per-day)
        clause (at-most (map #(occurs-at-hour % [day hour])
                              events-for-class) 1)]
    clause))

(defn teacher-clash-constraints
  [{:keys [teachers events num-days hours-per-day]}]
  (for [[teacher _] teachers
        :let [events-for-teacher
              (for [[e d] (events-to-schedule events)
                    :when ((:teachers d) teacher)]
                e)]
        :when (seq events-for-teacher)
        day (range num-days)
        hour (range hours-per-day)
        clause (at-most (map #(occurs-at-hour % [day hour])
                              events-for-teacher) 1)]
    clause))


(defn room-clash-constraints
  [{:keys [buildings events num-days hours-per-day]}]
  (for [[b rooms] buildings
        [r _] rooms
        :let [events-for-room
              (for [[e d] (events-to-schedule events)
                    :when ((:rooms d) [b r])]
                e)]
        :when (seq events-for-room)
        day (range num-days)
        hour (range hours-per-day)
        clause (at-most (map #(occurs-at-hour % [day hour])
                              events-for-room) 1)]
    clause))

;; Verhindern\ von\ Doppelbelegungen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Vorbelegte%20R%C3%A4ume%20und%20Zeiten][Vorbelegte\ Räume\ und\ Zeiten:1]]

(defn predetermined-room-constraints
  [{:keys [events num-days hours-per-day]}]
  (for [[event {rooms :rooms}] (events-to-schedule events)
        room rooms
        :when room]
    #{(occurs-at-room event room)}))

(defn predetermined-times-constraints
  [{:keys [events num-days hours-per-day]}]
  (for [[event {time :time}] (events-to-schedule events)
        :when time]
    #{(occurs-at-hour event time)}))

;; Vorbelegte\ Räume\ und\ Zeiten:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beachten%20von%20Zeitbeschr%C3%A4nkungen][Beachten\ von\ Zeitbeschränkungen:1]]

(defn rooms-not-available-constraints
  [{:keys [events buildings num-days hours-per-day]}]
  (for [[building rooms] buildings
        [room details] rooms
        :let [times-not-available (:times-not-available details)
              events-for-room
              (for [[e d] (events-to-schedule events)
                    :when ((:rooms d) [building room])]
                e)]
        event events-for-room
        time times-not-available]
    #{(negate (occurs-at-hour event time))}))

;; Beachten\ von\ Zeitbeschränkungen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Events%20mit%20l%C3%A4ngerer%20Dauer][Events\ mit\ längerer\ Dauer:1]]

(defn starts-at-hour
[event [day hour]]
(gen-var (symbol (str "starts-at-hour_" (name event) "_" day "_" hour))))

;; Events\ mit\ längerer\ Dauer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Events%20mit%20l%C3%A4ngerer%20Dauer][Events\ mit\ längerer\ Dauer:1]]

(defn starts-at-hour-occurs-at-hour-relations
  [{:keys [events num-days hours-per-day]}]
  (concat
   (for [[event {:keys [duration]}] (events-to-schedule events)
         day (range 0 num-days)
         hour (range 0 hours-per-day)
         [nd nh] (cons [day hour]
                       (next-times-at-day [day hour] hours-per-day (dec duration)))]
     (implies (starts-at-hour event [day hour])
              (occurs-at-hour event [nd nh])))
   (for [[event {:keys [duration]}] (events-to-schedule events)
         day (range 0 num-days)
         hour (range 0 hours-per-day)
         :let [starting-times
               (cons [day hour]
                     (previous-times-at-day [day hour] (dec duration)))]]
     (implies (occurs-at-hour event [day hour])
              (into #{} (map #(starts-at-hour event %) starting-times))))))

;; Events\ mit\ längerer\ Dauer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Events%20mit%20l%C3%A4ngerer%20Dauer][Events\ mit\ längerer\ Dauer:1]]

(deftest test-starts-at-hour-occurs-at-hour-relations
  (is (= '(#{{:negated? false, :var occurs-at-hour_bla:0_0_0}
   {:negated? true, :var starts-at-hour_bla:0_0_0}}
 #{{:negated? false, :var occurs-at-hour_bla:0_0_1}
   {:negated? true, :var starts-at-hour_bla:0_0_0}}
 #{{:negated? false, :var occurs-at-hour_bla:0_0_1}
   {:negated? true, :var starts-at-hour_bla:0_0_1}}
 #{{:negated? false, :var occurs-at-hour_bla:0_0_2}
   {:negated? true, :var starts-at-hour_bla:0_0_1}}
 #{{:negated? false, :var occurs-at-hour_bla:0_0_2}
   {:negated? true, :var starts-at-hour_bla:0_0_2}}
 #{{:negated? true, :var occurs-at-hour_bla:0_0_0}
   {:negated? false, :var starts-at-hour_bla:0_0_0}}
 #{{:negated? true, :var occurs-at-hour_bla:0_0_1}
   {:negated? false, :var starts-at-hour_bla:0_0_1}
   {:negated? false, :var starts-at-hour_bla:0_0_0}}
 #{{:negated? true, :var occurs-at-hour_bla:0_0_2}
   {:negated? false, :var starts-at-hour_bla:0_0_1}
   {:negated? false, :var starts-at-hour_bla:0_0_2}})
         (starts-at-hour-occurs-at-hour-relations {:num-days 1 :hours-per-day 3 :events {:bla (create-event :durations [2] :number-of-times 1)}}))))

(test-starts-at-hour-occurs-at-hour-relations)

;; Events\ mit\ längerer\ Dauer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Unm%C3%B6gliche%20Zeiten%20f%C3%BCr%20l%C3%A4ngere%20Veranstaltungen][Unmögliche\ Zeiten\ für\ längere\ Veranstaltungen:1]]

(defn illegal-starting-times-constraints
  [{:keys [events num-days hours-per-day]}]
  (for [[event {:keys [duration]}] (events-to-schedule events)
        hour (range (dec hours-per-day) (- hours-per-day duration) -1)
        day (range num-days)]
    #{(negate (starts-at-hour event [day hour]))}))

;; Unmögliche\ Zeiten\ für\ längere\ Veranstaltungen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Unm%C3%B6gliche%20Zeiten%20f%C3%BCr%20l%C3%A4ngere%20Veranstaltungen][Unmögliche\ Zeiten\ für\ längere\ Veranstaltungen:1]]

(deftest test-illegal-starting-times-constraints
  (is (= '(#{{:negated? true, :var starts-at-hour_bla:0_0_2}}
 #{{:negated? true, :var starts-at-hour_bla:0_1_2}})
          (illegal-starting-times-constraints {:num-days 2 :hours-per-day 3 :events {:bla (create-event :durations [2] :number-of-times 1)}}))))


(test-illegal-starting-times-constraints)

;; Unmögliche\ Zeiten\ für\ längere\ Veranstaltungen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beachten%20von%20Wechselzeiten%20zwischen%20Geb%C3%A4uden][Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1]]

(defn distances-constraints
  [{:keys [events buildings num-days hours-per-day distances]}]
  (for [[event {:keys [classes teachers rooms]}] (events-to-schedule events)
        :let [building (ffirst rooms)]
        [e dist] (for [[e {rs :rooms cls :classes tchs :teachers}]
                       (events-to-schedule events)
                       :let [b (ffirst rs)
                             distance (get distances [building b] 0)]
                       :when (and (not= b building) (not= event e)
                                  (> distance 0)
                                  (or (some classes cls) (some teachers tchs)))]
                   [e distance])
        day (range num-days)
        hour (range hours-per-day)
        [nd nh] (next-times-at-day [day hour] hours-per-day dist)]
    (implies (occurs-at-hour event [day hour])
             (negate (occurs-at-hour e [nd nh])))))

;; Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Jedes%20zu%20belegende%20Event%20muss%20belegt%20werden][Jedes\ zu\ belegende\ Event\ muss\ belegt\ werden:1]]

(defn everything-scheduled-constraints
  [{:keys [events num-days hours-per-day]}]
  (for [[event _] (events-to-schedule events)
        clause (exactly (for [day (range num-days)
                       hour (range hours-per-day)]
                   (starts-at-hour event [day hour])) 1)]
    clause))

;; Jedes\ zu\ belegende\ Event\ muss\ belegt\ werden:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Sammeln%20aller%20Constraint%20funktions][Sammeln\ aller\ Constraint\ funktions:1]]

(def constraint-functions
  [#'occurs-at-day-occurs-at-hour-relations
   #'occurs-at-hour-teached-at-relations
   #'class-clash-constraints
   #'room-clash-constraints
   #'teacher-clash-constraints
   #'everything-scheduled-constraints
   #'predetermined-room-constraints
   #'predetermined-times-constraints
   #'rooms-not-available-constraints
   #'distances-constraints
   #'starts-at-hour-occurs-at-hour-relations
   #'illegal-starting-times-constraints
   #'occurs-at-hour-teaches-at-relations])

(defn run-constraints
  ([entities] (run-constraints entities constraint-functions))
  ([entities constraint-functions]
   (reduce (fn [clauses cf]
             (println "creating constraints from " cf)
             (concat clauses (doall (cf entities))))
           [] constraint-functions)))

(def last-assignment (atom nil))

(defn solve-problem
  ([entities] (solve-problem
               entities {:constraint-functions constraint-functions :maxsat? true}))
  ([entities {:keys [constraint-functions maxsat?]
              :or {constraint-functions constraint-functions
                    maxsat? true}}]
   (let [_ (prn "encoding constraints")
         constraints (doall (run-constraints entities constraint-functions))
         _ (prn "get-assignment")
         ass (get-assignment constraints "resources/cnf1" maxsat?)
         _ (reset! last-assignment ass)]
     (when ass 
         (complete-entities entities ass)))))

;; Sammeln\ aller\ Constraint\ funktions:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def basic-example-problem
  {
   :classes {:class-1 {}
             :class-2 {}}
   :teachers {:teacher-1 {}
              :teacher-2 {}}
   :buildings {:building-1 {:room-1 {}
                            :room-2 {}}
               :building-2 {:room-1 {}
                            :room-2 {}}}
   :num-days 1
   :hours-per-day 2
   :distances {}
   :events
   {:class-1-math
    (create-event :classes :class-1 :rooms #{[:building-1 :room-1]}
                  :teachers #{:teacher-1} :number-of-times 1)
    :class-1-deutsch
    (create-event :classes :class-1 :rooms #{[:building-2 :room-1]}
                  :teachers #{:teacher-1} :number-of-times 1)
    :class-2-math
    (create-event :classes :class-2 :rooms #{[:building-2 :room-1]}
                  :teachers #{:teacher-2} :number-of-times 1)
    :class-2-deutsch
    (create-event :classes :class-2 :rooms #{[:building-1 :room-1]}
                  :teachers #{:teacher-2} :number-of-times 1)}
   })

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(deftest test-basic-example
  (is (= {:classes {:class-1 {}, :class-2 {}}, :teachers {:teacher-1 {}, :teacher-2 {}}, :buildings {:building-1 {:room-1 {}, :room-2 {}}, :building-2 {:room-1 {}, :room-2 {}}}, :num-days 1, :hours-per-day 2, :distances {}, :events {:class-1-math {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]}], :teachers [#{:teacher-1}], :durations [1], :allowed-rooms :all, :times [[0 1]]}, :class-1-deutsch {:classes #{:class-1}, :rooms [#{[:building-2 :room-1]}], :teachers [#{:teacher-1}], :durations [1], :allowed-rooms :all, :times [[0 0]]}, :class-2-math {:classes #{:class-2}, :rooms [#{[:building-2 :room-1]}], :teachers [#{:teacher-2}], :durations [1], :allowed-rooms :all, :times [[0 1]]}, :class-2-deutsch {:classes #{:class-2}, :rooms [#{[:building-1 :room-1]}], :teachers [#{:teacher-2}], :durations [1], :allowed-rooms :all, :times [[0 0]]}}}
         (solve-problem basic-example-problem {:maxsat? false}))))


(test-basic-example)

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def failing-example
  (assoc basic-example-problem
         :distances {[:building-1 :building-2] 1
                     [:building-2 :building-1] 1}))

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(deftest test-failing-example-fails
(is (nil? (solve-problem failing-example))))

(test-failing-example-fails)

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def enough-time-example
  (assoc failing-example
         :hours-per-day 3))

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(deftest test-enough-time
  (is (= {:classes {:class-1 {}, :class-2 {}}, :teachers {:teacher-1 {}, :teacher-2 {}}, :buildings {:building-1 {:room-1 {}, :room-2 {}}, :building-2 {:room-1 {}, :room-2 {}}}, :num-days 1, :hours-per-day 3, :distances {[:building-2 :building-1] 1, [:building-1 :building-2] 1}, :events {:class-1-math {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]}], :teachers [#{:teacher-1}], :durations [1], :allowed-rooms :all, :times [[0 0]]}, :class-1-deutsch {:classes #{:class-1}, :rooms [#{[:building-2 :room-1]}], :teachers [#{:teacher-1}], :durations [1], :allowed-rooms :all, :times [[0 2]]}, :class-2-math {:classes #{:class-2}, :rooms [#{[:building-2 :room-1]}], :teachers [#{:teacher-2}], :durations [1], :allowed-rooms :all, :times [[0 0]]}, :class-2-deutsch {:classes #{:class-2}, :rooms [#{[:building-1 :room-1]}], :teachers [#{:teacher-2}], :durations [1], :allowed-rooms :all, :times [[0 2]]}}} (solve-problem enough-time-example))))

(test-enough-time)

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(deftest test-basic-example
  (is (= {:classes {:class-1 {}, :class-2 {}}, :teachers {:teacher-1 {}, :teacher-2 {}}, :buildings {:building-1 {:room-1 {}, :room-2 {}}, :building-2 {:room-1 {}, :room-2 {}}}, :num-days 1, :hours-per-day 2, :distances {}, :events {:class-1-math {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]}], :teachers [#{:teacher-1}], :durations [1], :allowed-rooms :all, :times [[0 1]]}, :class-1-deutsch {:classes #{:class-1}, :rooms [#{[:building-2 :room-1]}], :teachers [#{:teacher-1}], :durations [1], :allowed-rooms :all, :times [[0 0]]}, :class-2-math {:classes #{:class-2}, :rooms [#{[:building-2 :room-1]}], :teachers [#{:teacher-2}], :durations [1], :allowed-rooms :all, :times [[0 1]]}, :class-2-deutsch {:classes #{:class-2}, :rooms [#{[:building-1 :room-1]}], :teachers [#{:teacher-2}], :durations [1], :allowed-rooms :all, :times [[0 0]]}}}
         (solve-problem basic-example-problem {:maxsat? false}))))


(test-basic-example)

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Untersuchung%20der%20Skalierbarkeit][Untersuchung\ der\ Skalierbarkeit:1]]

(defn create-scalable-problem [num-hours num-days num-classes]
  (let [num-slots (* num-days num-hours)]
    {:classes (reduce #(assoc %1 (keyword (str "class-" %2))
                              {:class-teacher (keyword (str "teacher-" %2))
                               :class-room [(keyword (str "building-" %2)) :room-1]})
                      {} (range num-classes))
     :teachers (reduce #(assoc %1 (keyword (str "teacher-" %2)) {}) {} (range num-classes))
     :distances {}
     :num-days num-days
     :hours-per-day num-hours
     :buildings (reduce #(assoc %1 (keyword (str "building-" %2)) {:room-1 {}})
                        {} (range num-classes))
     :events (reduce #(assoc %1 (keyword (str "ef-" %2))
                             (create-event :classes (keyword (str "class-" %2))
                                           :rooms #{[(keyword (str "building-" %2)) :room-1]}
                                           :teachers #{(keyword (str "teacher-" %2))}
                                           :number-of-times num-slots))
                     {} (range num-classes))}))

;; Untersuchung\ der\ Skalierbarkeit:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20L%C3%BCcken%20zwischen%20Veranstaltungen%20f%C3%BCr%20Klassen][Minimierung\ der\ Lücken\ zwischen\ Veranstaltungen\ für\ Klassen:1]]

(defn minimize-gaps-for-classes-constraints
  ([entities] (minimize-gaps-for-classes-constraints entities 1))
  ([{:keys [classes num-days hours-per-day]} cost]
   (for [[class _] classes
         day (range num-days)
         x (range 1 (dec hours-per-day))
         hour (range (- hours-per-day x 1))]
     [(set/union #{(negate (teached-at class [day hour]))
                   (negate (teached-at class [day (+ hour x 1)]))}
                 (into #{} (map #(teached-at class [day %])
                                (range (inc hour) (+ hour x 1))))) (* x cost)])))

;; Minimierung\ der\ Lücken\ zwischen\ Veranstaltungen\ für\ Klassen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20L%C3%BCcken%20zwischen%20Veranstaltungen%20f%C3%BCr%20Klassen][Minimierung\ der\ Lücken\ zwischen\ Veranstaltungen\ für\ Klassen:1]]

(defn old-minimize-gaps-for-classes-constraints
  ([entities] (minimize-gaps-for-classes-constraints entities 1))
  ([{:keys [classes num-days hours-per-day]} cost]
   (for [[class _] classes
         day (range num-days)
         hour (range hours-per-day)
         clause (cond
                  (= 0 hour)
                  [(implies (teached-at class [day 0])
                            (teached-at class [day 1]))]
                  (= (dec hours-per-day) hour)
                  [(implies (teached-at class [day hour])
                            (teached-at class [day (dec hour)]))]
                  :else
                  [(implies (teached-at class [day hour])
                            (teached-at class [day (dec hour)]))
                   (implies (teached-at class [day hour])
                            (teached-at class [day (inc hour)]))])]
     [clause cost])))

;; Minimierung\ der\ Lücken\ zwischen\ Veranstaltungen\ für\ Klassen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20L%C3%BCcken%20zwischen%20Veranstaltungen%20f%C3%BCr%20Lehrer][Minimierung\ der\ Lücken\ zwischen\ Veranstaltungen\ für\ Lehrer:1]]

(defn minimize-gaps-for-teachers-constraints
  ([entities] (minimize-gaps-for-teachers-constraints entities 1))
  ([{:keys [teachers num-days hours-per-day]} cost]
   (for [[teacher _] teachers
         day (range num-days)
         x (range 1 (dec hours-per-day))
         hour (range (- hours-per-day x 1))]
     [(set/union #{(negate (teaches-at teacher [day hour]))
                   (negate (teaches-at teacher [day (+ hour x 1)]))}
                 (into #{} (map #(teaches-at teacher [day %])
                                (range (inc hour) (+ hour x 1))))) (* x cost)])))

;; Minimierung\ der\ Lücken\ zwischen\ Veranstaltungen\ für\ Lehrer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20L%C3%BCcken%20zwischen%20Veranstaltungen%20f%C3%BCr%20Lehrer][Minimierung\ der\ Lücken\ zwischen\ Veranstaltungen\ für\ Lehrer:1]]

(defn old-minimize-gaps-for-teachers-constraints
  ([entities] (minimize-gaps-for-teachers-constraints entities 1))
  ([{:keys [teachers num-days hours-per-day]} cost]
   (for [[teacher _] teachers
         day (range num-days)
         hour (range hours-per-day)
         clause (cond
                  (= 0 hour)
                  [(implies (teaches-at teacher [day 0])
                            (teaches-at teacher [day 1]))]
                  (= (dec hours-per-day) hour)
                  [(implies (teaches-at teacher [day hour])
                            (teaches-at teacher [day (dec hour)]))]
                  :else
                  [(implies (teaches-at teacher [day hour])
                            (teaches-at teacher [day (dec hour)]))
                   (implies (teaches-at teacher [day hour])
                            (teaches-at teacher [day (inc hour)]))])]
     [clause cost])))

;; Minimierung\ der\ Lücken\ zwischen\ Veranstaltungen\ für\ Lehrer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20Wechsel%20zwischen%20Geb%C3%A4uden%20f%C3%BCr%20Klassen][Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Klassen:1]]

(defn class-in-building [class [day hour] building]
  (gen-var (symbol (str "class-in-building_" (name class) "_" day "_"
                        hour "_" (name building)))))

;; Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Klassen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20Wechsel%20zwischen%20Geb%C3%A4uden%20f%C3%BCr%20Klassen][Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Klassen:1]]

(defn class-in-building-relations
  [{:keys [events buildings num-days hours-per-day classes]}]
  (concat
   (for [[event {:keys [classes rooms]}] (events-to-schedule events)
         class classes
         day (range num-days)
         hour (range hours-per-day)]
     (implies (occurs-at-hour event [day hour])
              (class-in-building class [day hour] (ffirst rooms))))
   (for [[building _] buildings
         [class _] classes
         :let [events
               (for [[event {classes :classes rooms :rooms}] (events-to-schedule events)
                     :when (and (classes class) (= building (ffirst rooms)))]
                 event)]
         day (range num-days)
         hour (range hours-per-day)
         :let [event-occurrences (map #(occurs-at-hour % [day hour]) events)]
         clause (if (> hour 0)
                  [#{(negate (class-in-building class [day (dec hour)] building))
                      (teached-at class [day hour])
                      (class-in-building class [day hour] building)}
                   (implies (class-in-building class [day hour] building)
                            (into #{} (concat event-occurrences
                                              [(class-in-building class [day (dec hour)] building)])))
                   (implies (class-in-building class [day hour] building)
                            (into #{} (concat event-occurrences
                                              [(negate (teached-at class [day hour]))])))]
                  [(implies (class-in-building class [day hour] building)
                            (into #{} event-occurrences))])]
     clause)))

;; Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Klassen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20Wechsel%20zwischen%20Geb%C3%A4uden%20f%C3%BCr%20Lehrer][Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Lehrer:1]]

(defn teacher-in-building [teacher [day hour] building]
(gen-var (symbol (str "teacher-in-building_" (name teacher) "_" day "_" hour "_" (name building)))))

;; Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Lehrer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20Wechsel%20zwischen%20Geb%C3%A4uden%20f%C3%BCr%20Lehrer][Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Lehrer:1]]

(defn minimize-building-changes-constraints-for-classes
  ([entities]  (minimize-building-changes-constraints-for-classes entities (partial * 20)))
  ([{:keys [classes buildings num-days hours-per-day distances]} cost-fn]
   (for [[class _] classes
         [building-1 _] buildings
         [building-2 _] buildings
         :let [distance (get distances [building-1 building-2] 0)]
         :when (and (not= building-1 building-2) (> distance 0))
         day (range num-days)
         hour (range (dec hours-per-day))]
     [#{(negate (class-in-building class [day hour] building-1))
        (negate (class-in-building class [day (inc hour)] building-2))}
      (cost-fn distance)])))

;; Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Lehrer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20Wechsel%20zwischen%20Geb%C3%A4uden%20f%C3%BCr%20Lehrer][Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Lehrer:1]]

(defn teacher-in-building-relations
  [{:keys [events buildings num-days hours-per-day teachers]}]
  (concat
   (for [[event {:keys [teachers rooms]}] (events-to-schedule events)
         teacher teachers
         day (range num-days)
         hour (range hours-per-day)]
     (implies (occurs-at-hour event [day hour])
              (teacher-in-building teacher [day hour] (ffirst rooms))))
   (for [[building _] buildings
         [teacher _] teachers
         :let [events
               (for [[event {teachers :teachers rooms :rooms}] (events-to-schedule events)
                     :when (and (teachers teacher) (= building (ffirst rooms)))]
                 event)]
         day (range num-days)
         hour (range hours-per-day)
         :let [event-occurrences (map #(occurs-at-hour % [day hour]) events)]
         clause (if (> hour 0)
                  [#{(negate (teacher-in-building teacher [day (dec hour)] building))
                      (teaches-at teacher [day hour])
                      (teacher-in-building teacher [day hour] building)}
                   (implies (teacher-in-building teacher [day hour] building)
                            (into #{} (concat event-occurrences
                                              [(teacher-in-building teacher [day (dec hour)] building)])))
                   (implies (teacher-in-building teacher [day hour] building)
                            (into #{} (concat event-occurrences
                                              [(negate (teaches-at teacher [day hour]))])))]
                  [(implies (teacher-in-building teacher [day hour] building)
                            (into #{} event-occurrences))])]
     clause)))

;; Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Lehrer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Minimierung%20der%20Wechsel%20zwischen%20Geb%C3%A4uden%20f%C3%BCr%20Lehrer][Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Lehrer:1]]

(defn minimize-building-changes-constraints-for-teachers
  ([entities]  (minimize-building-changes-constraints-for-teachers entities (partial * 20)))
  ([{:keys [teachers buildings num-days hours-per-day distances]} cost-fn]
   (for [[teacher _] teachers
         [building-1 _] buildings
         [building-2 _] buildings
         :let [distance (get distances [building-1 building-2] 0)]
         :when (and (not= building-1 building-2) (> distance 0))
         day (range num-days)
         hour (range (dec hours-per-day))]
     [#{(negate (teacher-in-building teacher [day hour] building-1))
        (negate (teacher-in-building teacher [day (inc hour)] building-2))}
      (cost-fn distance)])))

;; Minimierung\ der\ Wechsel\ zwischen\ Gebäuden\ für\ Lehrer:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Optimal%20constraints][Optimal\ constraints:1]]

(def optimal-constraint-functions
        (concat constraint-functions
                [#'minimize-gaps-for-classes-constraints
                 #'minimize-gaps-for-teachers-constraints
                 #'class-in-building-relations
                 #'teacher-in-building-relations
                 #'minimize-building-changes-constraints-for-classes
                 #'minimize-building-changes-constraints-for-teachers]))

      (defn solve-problem-optimally [entities]
        (solve-problem entities {:constraint-functions optimal-constraint-functions :maxsat? true}))

        (deftest test-solves
(is (not (nil? (solve-problem example-entities))))
(is (not (nil? (solve-problem bigger-example-entities)))))

;; Optimal\ constraints:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def minimize-gaps-example-entities
  {
   :classes {:class-1 {}}
   :teachers {:teacher-1 {}}
   :buildings {:building-1 {:room-1 {:times-not-available #{[0 0] [0 2] [0 5] [0 6]}}}}
   :num-days 1
   :hours-per-day 10
   :distances {}
   :events
   {:class-1-math
    (create-event :classes :class-1 :rooms #{[:building-1 :room-1]}
                  :teachers #{:teacher-1} :number-of-times 3)}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(deftest test-minimize-gaps-example
  (is (= {:classes {:class-1 {}}, :teachers {:teacher-1 {}}, :buildings {:building-1 {:room-1 {:times-not-available #{[0 0] [0 6] [0 5] [0 2]}}}}, :num-days 1, :hours-per-day 10, :distances {}, :events {:class-1-math {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1], :allowed-rooms :all, :times [[0 3] [0 7] [0 9]]}}}
         (solve-problem minimize-gaps-example-entities)))
  (is (= {:classes {:class-1 {}}, :teachers {:teacher-1 {}}, :buildings {:building-1 {:room-1 {:times-not-available #{[0 0] [0 6] [0 5] [0 2]}}}}, :num-days 1, :hours-per-day 10, :distances {}, :events {:class-1-math {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1], :allowed-rooms :all, :times [[0 9] [0 8] [0 7]]}}}
         (solve-problem-optimally minimize-gaps-example-entities))))


(test-minimize-gaps-example)

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def minimize-buildings-example
  {
   :classes {:class-1 {}}
   :teachers {:teacher-1 {}}
   :buildings {:building-1 {:room-1 {}}
               :building-2 {:room-1 {}}
               :building-3 {:room-1 {}}}
   :distances {[:building-1 :building-2] 2
               [:building-2 :building-1] 2
               [:building-1 :building-3] 1
               [:building-3 :building-1] 1
               [:building-2 :building-3] 1
               [:building-3 :building-2] 1}
   :num-days 1
   :hours-per-day 10
   :events {:class-1-math
            (create-event :classes :class-1 :rooms #{[:building-1 :room-1]}
                          :teachers #{:teacher-1} :number-of-times 2)
            :class-1-german
            (create-event :classes :class-1 :rooms #{[:building-2 :room-1]}
                          :teachers #{:teacher-1} :number-of-times 2)
            :class-1-art
            (create-event :classes :class-1 :rooms #{[:building-3 :room-1]}
                          :teachers #{:teacher-1} :number-of-times 2)}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def bigger-example-optimal-results
  {:classes {:class-4 {:class-teacher :teacher-4, :class-room [:building-2 :room-2]}, :class-10 {:class-teacher :teacher-10, :class-room [:building-2 :room-5]}, :class-11 {:class-teacher :teacher-11, :class-room [:building-1 :room-6]}, :class-8 {:class-teacher :teacher-8, :class-room [:building-2 :room-4]}, :class-2 {:class-teacher :teacher-2, :class-room [:building-2 :room-1]}, :class-12 {:class-teacher :teacher-12, :class-room [:building-2 :room-6]}, :class-3 {:class-teacher :teacher-3, :class-room [:building-1 :room-2]}, :class-5 {:class-teacher :teacher-5, :class-room [:building-1 :room-3]}, :class-1 {:class-teacher :teacher-1, :class-room [:building-1 :room-1]}, :class-7 {:class-teacher :teacher-7, :class-room [:building-1 :room-4]}, :class-6 {:class-teacher :teacher-6, :class-room [:building-2 :room-3]}, :class-9 {:class-teacher :teacher-9, :class-room [:building-1 :room-5]}}, :events {:class-9-werken {:classes #{:class-9}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-9} #{:teacher-9}], :durations [1 1], :allowed-rooms :all, :times [[2 5] [1 2]]}, :class-2-werken {:classes #{:class-2}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-2} #{:teacher-2}], :durations [1 1], :allowed-rooms :all, :times [[0 0] [0 2]]}, :class-10-sport {:classes #{:class-10}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[3 3]]}, :class-5-art {:classes #{:class-5}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-5} #{:teacher-5}], :durations [1 1], :allowed-rooms :all, :times [[2 2] [2 0]]}, :class-3-german {:classes #{:class-3}, :rooms [#{[:building-1 :room-2]} #{[:building-1 :room-2]} #{[:building-1 :room-2]} #{[:building-1 :room-2]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 1] [4 3] [4 2] [1 0]]}, :class-7-werken {:classes #{:class-7}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-7} #{:teacher-7}], :durations [1 1], :allowed-rooms :all, :times [[0 5] [1 5]]}, :class-3-math {:classes #{:class-3}, :rooms [#{[:building-1 :room-2]} #{[:building-1 :room-2]} #{[:building-1 :room-2]} #{[:building-1 :room-2]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 4] [3 5] [1 5] [2 5]]}, :class-2-art {:classes #{:class-2}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-2} #{:teacher-2}], :durations [1 1], :allowed-rooms :all, :times [[3 0] [4 5]]}, :class-3-werken {:classes #{:class-3}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-3} #{:teacher-3}], :durations [1 1], :allowed-rooms :all, :times [[2 2] [2 3]]}, :class-1-sk {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :allowed-rooms :all, :times [[2 5] [2 4] [0 3] [0 2]]}, :class-8-math {:classes #{:class-8}, :rooms [#{[:building-2 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-4]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :allowed-rooms :all, :times [[0 5] [2 2] [0 3] [2 1]]}, :class-1-art {:classes #{:class-1}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-1} #{:teacher-1}], :durations [1 1], :allowed-rooms :all, :times [[0 4] [1 5]]}, :class-6-werken {:classes #{:class-6}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-6} #{:teacher-6}], :durations [1 1], :allowed-rooms :all, :times [[3 0] [4 2]]}, :class-10-art {:classes #{:class-10}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-10} #{:teacher-10}], :durations [1 1], :allowed-rooms :all, :times [[2 3] [4 3]]}, :class-9-sport {:classes #{:class-9}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[2 2]]}, :class-12-art {:classes #{:class-12}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-12} #{:teacher-12}], :durations [1 1], :allowed-rooms :all, :times [[1 1] [1 2]]}, :class-1-sport {:classes #{:class-1}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[1 2]]}, :class-9-art {:classes #{:class-9}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-9} #{:teacher-9}], :durations [1 1], :allowed-rooms :all, :times [[0 5] [0 2]]}, :class-10-werken {:classes #{:class-10}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-10} #{:teacher-10}], :durations [1 1], :allowed-rooms :all, :times [[0 3] [1 0]]}, :class-8-sport {:classes #{:class-8}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[1 0]]}, :class-3-sk {:classes #{:class-3}, :rooms [#{[:building-1 :room-2]} #{[:building-1 :room-2]} #{[:building-1 :room-2]} #{[:building-1 :room-2]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :allowed-rooms :all, :times [[3 4] [3 2] [2 0] [1 2]]}, :class-11-werken {:classes #{:class-11}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-11} #{:teacher-11}], :durations [1 1], :allowed-rooms :all, :times [[1 4] [2 4]]}, :class-7-math {:classes #{:class-7}, :rooms [#{[:building-1 :room-4]} #{[:building-1 :room-4]} #{[:building-1 :room-4]} #{[:building-1 :room-4]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :allowed-rooms :all, :times [[2 2] [2 3] [4 1] [3 5]]}, :class-8-art {:classes #{:class-8}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-8} #{:teacher-8}], :durations [1 1], :allowed-rooms :all, :times [[4 2] [0 0]]}, :class-4-art {:classes #{:class-4}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-4} #{:teacher-4}], :durations [1 1], :allowed-rooms :all, :times [[3 4] [0 3]]}, :class-7-german {:classes #{:class-7}, :rooms [#{[:building-1 :room-4]} #{[:building-1 :room-4]} #{[:building-1 :room-4]} #{[:building-1 :room-4]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 4] [2 5] [4 5] [0 2]]}, :class-6-sport {:classes #{:class-6}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[0 4]]}, :class-11-sport {:classes #{:class-11}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[4 1]]}, :class-2-german {:classes #{:class-2}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :allowed-rooms :all, :times [[1 0] [1 5] [3 3] [0 1]]}, :class-7-art {:classes #{:class-7}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-7} #{:teacher-7}], :durations [1 1], :allowed-rooms :all, :times [[2 4] [1 0]]}, :class-4-math {:classes #{:class-4}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-2]} #{[:building-2 :room-2]} #{[:building-2 :room-2]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :allowed-rooms :all, :times [[1 0] [2 4] [4 2] [1 3]]}, :class-6-german {:classes #{:class-6}, :rooms [#{[:building-2 :room-3]} #{[:building-2 :room-3]} #{[:building-2 :room-3]} #{[:building-2 :room-3]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 4] [0 0] [4 1] [3 3]]}, :class-4-german {:classes #{:class-4}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-2]} #{[:building-2 :room-2]} #{[:building-2 :room-2]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :allowed-rooms :all, :times [[2 3] [1 5] [1 4] [1 2]]}, :class-1-german {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 4] [4 5] [4 2] [3 0]]}, :class-11-math {:classes #{:class-11}, :rooms [#{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-1 :room-6]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :allowed-rooms :all, :times [[0 3] [0 2] [3 3] [3 1]]}, :class-11-art {:classes #{:class-11}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-11} #{:teacher-11}], :durations [1 1], :allowed-rooms :all, :times [[2 1] [0 1]]}, :class-2-math {:classes #{:class-2}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :allowed-rooms :all, :times [[1 2] [2 0] [1 3] [4 3]]}, :class-12-werken {:classes #{:class-12}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-12} #{:teacher-12}], :durations [1 1], :allowed-rooms :all, :times [[0 1] [3 1]]}, :class-6-sk {:classes #{:class-6}, :rooms [#{[:building-2 :room-3]} #{[:building-2 :room-3]} #{[:building-2 :room-3]} #{[:building-2 :room-3]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 0] [3 2] [1 5] [2 1]]}, :class-4-sport {:classes #{:class-4}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[2 0]]}, :class-11-sk {:classes #{:class-11}, :rooms [#{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-1 :room-6]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :allowed-rooms :all, :times [[0 0] [2 2] [2 0] [0 4]]}, :class-2-sport {:classes #{:class-2}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[2 4]]}, :class-2-sk {:classes #{:class-2}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :allowed-rooms :all, :times [[1 1] [4 1] [0 5] [0 3]]}, :class-8-werken {:classes #{:class-8}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-8} #{:teacher-8}], :durations [1 1], :allowed-rooms :all, :times [[2 0] [0 4]]}, :class-5-werken {:classes #{:class-5}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-5} #{:teacher-5}], :durations [1 1], :allowed-rooms :all, :times [[3 3] [1 3]]}, :class-12-sk {:classes #{:class-12}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-6]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :allowed-rooms :all, :times [[0 2] [4 4] [0 0] [2 0]]}, :class-3-sport {:classes #{:class-3}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[0 0]]}, :class-7-sport {:classes #{:class-7}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[3 1]]}, :class-12-math {:classes #{:class-12}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-6]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 3] [2 4] [4 2] [2 2]]}, :class-4-werken {:classes #{:class-4}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-4} #{:teacher-4}], :durations [1 1], :allowed-rooms :all, :times [[1 1] [4 5]]}, :class-10-math {:classes #{:class-10}, :rooms [#{[:building-2 :room-5]} #{[:building-2 :room-5]} #{[:building-2 :room-5]} #{[:building-2 :room-5]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 1] [4 5] [0 4] [3 0]]}, :class-5-sk {:classes #{:class-5}, :rooms [#{[:building-1 :room-3]} #{[:building-1 :room-3]} #{[:building-1 :room-3]} #{[:building-1 :room-3]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :allowed-rooms :all, :times [[0 5] [3 0] [1 5] [2 5]]}, :class-9-german {:classes #{:class-9}, :rooms [#{[:building-1 :room-5]} #{[:building-1 :room-5]} #{[:building-1 :room-5]} #{[:building-1 :room-5]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :allowed-rooms :all, :times [[0 0] [3 4] [3 0] [3 1]]}, :class-3-art {:classes #{:class-3}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-3} #{:teacher-3}], :durations [1 1], :allowed-rooms :all, :times [[3 3] [1 4]]}, :class-9-sk {:classes #{:class-9}, :rooms [#{[:building-1 :room-5]} #{[:building-1 :room-5]} #{[:building-1 :room-5]} #{[:building-1 :room-5]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :allowed-rooms :all, :times [[3 2] [3 5] [3 3] [1 5]]}, :class-11-german {:classes #{:class-11}, :rooms [#{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-1 :room-6]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :allowed-rooms :all, :times [[3 5] [3 2] [3 4] [3 0]]}, :class-4-sk {:classes #{:class-4}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-2]} #{[:building-2 :room-2]} #{[:building-2 :room-2]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :allowed-rooms :all, :times [[0 0] [4 4] [4 3] [4 1]]}, :class-5-sport {:classes #{:class-5}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[4 4]]}, :class-6-art {:classes #{:class-6}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-6} #{:teacher-6}], :durations [1 1], :allowed-rooms :all, :times [[3 5] [2 5]]}, :class-9-math {:classes #{:class-9}, :rooms [#{[:building-1 :room-5]} #{[:building-1 :room-5]} #{[:building-1 :room-5]} #{[:building-1 :room-5]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :allowed-rooms :all, :times [[1 0] [0 4] [0 1] [0 3]]}, :class-8-german {:classes #{:class-8}, :rooms [#{[:building-2 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-4]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :allowed-rooms :all, :times [[3 4] [3 0] [3 5] [3 1]]}, :class-1-werken {:classes #{:class-1}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-1} #{:teacher-1}], :durations [1 1], :allowed-rooms :all, :times [[2 1] [3 5]]}, :class-5-math {:classes #{:class-5}, :rooms [#{[:building-1 :room-3]} #{[:building-1 :room-3]} #{[:building-1 :room-3]} #{[:building-1 :room-3]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :allowed-rooms :all, :times [[1 0] [0 1] [1 1] [0 2]]}, :class-10-german {:classes #{:class-10}, :rooms [#{[:building-2 :room-5]} #{[:building-2 :room-5]} #{[:building-2 :room-5]} #{[:building-2 :room-5]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :allowed-rooms :all, :times [[0 0] [0 1] [0 2] [2 1]]}, :class-7-sk {:classes #{:class-7}, :rooms [#{[:building-1 :room-4]} #{[:building-1 :room-4]} #{[:building-1 :room-4]} #{[:building-1 :room-4]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :allowed-rooms :all, :times [[1 3] [1 1] [0 3] [0 0]]}, :class-1-math {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :allowed-rooms :all, :times [[2 3] [0 5] [0 0] [0 1]]}, :class-12-german {:classes #{:class-12}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-6]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 5] [0 4] [2 5] [0 3]]}, :class-6-math {:classes #{:class-6}, :rooms [#{[:building-2 :room-3]} #{[:building-2 :room-3]} #{[:building-2 :room-3]} #{[:building-2 :room-3]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :allowed-rooms :all, :times [[4 5] [4 3] [3 1] [2 0]]}, :class-5-german {:classes #{:class-5}, :rooms [#{[:building-1 :room-3]} #{[:building-1 :room-3]} #{[:building-1 :room-3]} #{[:building-1 :room-3]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :allowed-rooms :all, :times [[2 3] [0 3] [0 4] [0 0]]}, :class-8-sk {:classes #{:class-8}, :rooms [#{[:building-2 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-4]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :allowed-rooms :all, :times [[3 2] [2 4] [2 3] [0 2]]}, :class-12-sport {:classes #{:class-12}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :allowed-rooms :all, :times [[1 4]]}, :class-10-sk {:classes #{:class-10}, :rooms [#{[:building-2 :room-5]} #{[:building-2 :room-5]} #{[:building-2 :room-5]} #{[:building-2 :room-5]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :allowed-rooms :all, :times [[1 5] [1 4] [2 0] [4 0]]}}, :buildings {:building-1 {:room-1 {}, :room-2 {}, :room-3 {}, :room-4 {}, :room-5 {}, :room-6 {}, :art-room {}}, :building-2 {:room-1 {}, :room-2 {}, :room-3 {}, :room-4 {}, :room-5 {}, :room-6 {}, :werkraum {}}, :sporthalle {:sporthalle {}}}, :num-days 5, :hours-per-day 6, :teachers {:teacher-7 {}, :teacher-6 {}, :teacher-10 {}, :teacher-1 {}, :teacher-11 {}, :sportlehrer {}, :teacher-4 {}, :teacher-2 {}, :teacher-12 {}, :teacher-8 {}, :teacher-3 {}, :teacher-5 {}, :teacher-9 {}}, :distances {[:building-1 :sporthalle] 1, [:sporthalle :building-2] 1, [:sporthalle :building-1] 1, [:building-2 :sporthalle] 1, [:building-2 :building-1] 1, [:building-1 :building-2] 1}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Untersuchung%20der%20Skalierbarkeit][Untersuchung\ der\ Skalierbarkeit:1]]

(defn create-scalable-optimizeable-problem [num-hours num-days num-classes]
  (let [num-slots (* num-days num-hours)]
    {:classes (reduce #(assoc %1 (keyword (str "class-" %2))
                              {:class-teacher (keyword (str "teacher-" %2))
                               :class-room [(keyword (str "building-" %2)) :room-1]})
                      {} (range num-classes))
     :teachers (reduce #(assoc %1 (keyword (str "teacher-" %2)) {}) {} (range num-classes))
     :distances {}
     :num-days num-days
     :hours-per-day num-hours
     :buildings (reduce #(assoc %1 (keyword (str "building-" %2)) {:room-1 {}})
                        {} (range num-classes))
     :events (reduce #(assoc %1 (keyword (str "ef-" %2))
                             (create-event :classes (keyword (str "class-" %2))
                                           :rooms #{[(keyword (str "building-" %2)) :room-1]}
                                           :teachers #{(keyword (str "teacher-" %2))}
                                           :number-of-times (dec num-slots)))
                     {} (range num-classes))}))

;; Untersuchung\ der\ Skalierbarkeit:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Jedes%20Event%20muss%20einem%20Raum%20zugeordnet%20sein][Jedes\ Event\ muss\ einem\ Raum\ zugeordnet\ sein:1]]

(defn everything-exactly-one-room-constraints
  [{:keys [events buildings]}]
  (for [[event _] (events-to-schedule events)
        clause (exactly (for [[building rooms] buildings
                              [room _] rooms]
                          (occurs-at-room event [building room])) 1)]
    clause))

;; Jedes\ Event\ muss\ einem\ Raum\ zugeordnet\ sein:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Eingrenzung%20der%20Raumwahl][Eingrenzung\ der\ Raumwahl:1]]

(defn forbidden-rooms-constraints
  [{:keys [events]}]
  (for [[event details] (events-to-schedule events)
        forbidden-room (:forbidden-rooms details)]
    #{(negate (occurs-at-room event forbidden-room))}))

;; Eingrenzung\ der\ Raumwahl:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Verhindern%20von%20Doppelbelegungen%20von%20R%C3%A4umen][Verhindern\ von\ Doppelbelegungen\ von\ Räumen:1]]

(defn occurs-at-room-at-hour
  [event [building room] [day hour]]
  (gen-var (symbol (str "occurs-at-handr_" (name event) "_" (name building) "_" (name room) "_" day "_" hour))))

;; Verhindern\ von\ Doppelbelegungen\ von\ Räumen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Verhindern%20von%20Doppelbelegungen%20von%20R%C3%A4umen][Verhindern\ von\ Doppelbelegungen\ von\ Räumen:1]]

(defn occurs-at-room-at-hour-occurs-at-room-relations
  [{:keys [events buildings num-days hours-per-day]}]
  (for [[event _] (events-to-schedule events)
        [building rooms] buildings
        [room _] rooms
        day (range num-days)
        hour (range hours-per-day)
        clause [(implies (occurs-at-room-at-hour event [building room] [day hour])
                         (occurs-at-hour event [day hour]))
                (implies (occurs-at-room-at-hour event [building room] [day hour])
                         (occurs-at-room event [building room]))
                #{(negate (occurs-at-hour event [day hour]))
                  (negate (occurs-at-room event [building room]))
                  (occurs-at-room-at-hour event [building room] [day hour])}]]
    clause))

;; Verhindern\ von\ Doppelbelegungen\ von\ Räumen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Verhindern%20von%20Doppelbelegungen%20von%20R%C3%A4umen][Verhindern\ von\ Doppelbelegungen\ von\ Räumen:1]]

(defn scheduled-room-clash-constraints
  [{:keys [buildings events num-days hours-per-day]}]
  (let [events (for [[event _] (events-to-schedule events)] event)]
    (for [[building rooms] buildings
          [room _] rooms
          day (range num-days)
          hour (range hours-per-day)
          clause (at-most (map #(occurs-at-room-at-hour
                                 % [building room] [day hour])
                               events) 1)]
      clause)))

;; Verhindern\ von\ Doppelbelegungen\ von\ Räumen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beachten%20von%20Zeitbeschr%C3%A4nkungen][Beachten\ von\ Zeitbeschränkungen:1]]

(defn scheduled-rooms-not-available-constraints
  [{:keys [events buildings num-days hours-per-day]}]
  (for [[event _] (events-to-schedule events)
        [building rooms] buildings
        [room details] rooms
        :let [times-not-available (:times-not-available details)]
        time times-not-available]
    #{(negate (occurs-at-room-at-hour event [building room] time))}))

;; Beachten\ von\ Zeitbeschränkungen:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beachten%20von%20Wechselzeiten%20zwischen%20Geb%C3%A4uden][Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1]]

(defn room-scheduling-distances-constraints 
  [{:keys [events buildings num-days hours-per-day distances]}]
  (let [to-schedule (events-to-schedule events)]
    (for [[event {:keys [classes teachers]}] to-schedule 
          :let [events-with-common-resource
                (for [[e {cls :classes tchs :teachers}] to-schedule
                      :when (and (not= event e)
                                 (or (some classes cls) (some teachers tchs)))] e)]
          [building-1 rooms-1] buildings
          :let [buildings-in-distance
                (for [[b rs] buildings
                      :let [distance (get distances [building-1 b] 0)]
                      :when (and (not= b building-1)
                                 (> distance 0))]
                  [b rs distance])]
          [building-2 rooms-2 dist] buildings-in-distance
          [room-1 _] rooms-1
          [room-2 _] rooms-2
          day (range num-days)
          hour (range hours-per-day)
          [nd nh] (next-times-at-day [day hour ] hours-per-day dist)
          event-2 events-with-common-resource]
      (implies (occurs-at-room-at-hour event [building-1 room-1] [day hour])
               (negate (occurs-at-room-at-hour event-2 [building-2 room-2] [nd nh]))))))

;; Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beachten%20von%20Wechselzeiten%20zwischen%20Geb%C3%A4uden][Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1]]

(defn scheduled-rooms-class-in-buildings-relations
  [{:keys [events buildings num-days hours-per-day classes]}]
  (concat
   (for [[event {:keys [classes]}] (events-to-schedule events)
         class classes
         [building rooms] buildings
         [room _] rooms
         day (range num-days)
         hour (range hours-per-day)]
     (implies (occurs-at-room-at-hour event [building room] [day hour])
              (class-in-building class [day hour] building)))
   (for [[class _] classes
         :let [events
               (for [[event {classes :classes}] (events-to-schedule events)
                     :when (classes class)]
                 event)]
         [building rooms] buildings
         day (range num-days)
         hour (range hours-per-day)
         :let [event-occurrences-in-building
               (for [[room _] rooms
                     event events]
                 (occurs-at-room-at-hour event [building room] [day hour]))]
         clause (if (> hour 0)
                  [#{(negate (class-in-building class [day (dec hour)] building))
                     (teached-at class [day hour])
                     (class-in-building class [day hour] building)}
                   (implies (class-in-building class [day hour] building)
                            (into #{} (concat event-occurrences-in-building
                                              [(class-in-building class [day (dec hour)]
                                                                  building)])))
                   (implies (class-in-building class [day hour] building)
                            (into #{} (concat event-occurrences-in-building
                                              [(negate (teached-at class [day hour]))])))]
                  [(implies (class-in-building class [day hour] building)
                            (into #{} event-occurrences-in-building))])]
     clause)))

;; Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beachten%20von%20Wechselzeiten%20zwischen%20Geb%C3%A4uden][Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1]]

(defn scheduled-rooms-teacher-in-buildings-relations
  [{:keys [events buildings num-days hours-per-day teachers]}]
  (concat
   (for [[event {:keys [teachers]}] (events-to-schedule events)
         teacher teachers
         [building rooms] buildings
         [room _] rooms
         day (range num-days)
         hour (range hours-per-day)]
     (implies (occurs-at-room-at-hour event [building room] [day hour])
              (teacher-in-building teacher [day hour] building)))
   (for [[teacher _] teachers
         :let [events
               (for [[event {teachers :teachers}] (events-to-schedule events)
                     :when (teachers teacher)]
                 event)]
         [building rooms] buildings
         day (range num-days)
         hour (range hours-per-day)
         :let [event-occurrences-in-building
               (for [[room _] rooms
                     event events]
                 (occurs-at-room-at-hour event [building room] [day hour]))]
         clause (if (> hour 0)
                  [#{(negate (teacher-in-building teacher [day (dec hour)] building))
                     (teaches-at teacher [day hour])
                     (teacher-in-building teacher [day hour] building)}
                   (implies (teacher-in-building teacher [day hour] building)
                            (into #{} (concat event-occurrences-in-building
                                              [(teacher-in-building teacher [day (dec hour)]
                                                                    building)])))
                   (implies (teacher-in-building teacher [day hour] building)
                            (into #{} (concat event-occurrences-in-building
                                              [(negate (teaches-at teacher [day hour]))])))]
                  [(implies (teacher-in-building teacher [day hour] building)
                            (into #{} event-occurrences-in-building))])]
     clause)))

;; Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beachten%20von%20Wechselzeiten%20zwischen%20Geb%C3%A4uden][Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1]]

(defn building-changes-constraints-for-classes
  [{:keys [events buildings num-days hours-per-day distances classes]}]
  (for [[class _] classes
        [building-1 rooms-1] buildings
        [building-2 rooms-2] buildings
        :let [distance (get distances [building-1 building-2] 0)]
        :when (and (not= building-1 building-2) (> distance 0))
        day (range num-days)
        hour (range hours-per-day)
        [nd nh] (next-times-at-day [day hour] hours-per-day distance)]
    #{(negate (class-in-building class [day hour] building-1))
      (negate (teached-at class [day hour]))
      (negate (class-in-building class [nd nh] building-2))}))

;; Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Beachten%20von%20Wechselzeiten%20zwischen%20Geb%C3%A4uden][Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1]]

(defn building-changes-constraints-for-teachers
  [{:keys [events buildings num-days hours-per-day distances teachers]}]
  (for [[teacher _] teachers
        [building-1 rooms-1] buildings
        [building-2 rooms-2] buildings
        :let [distance (get distances [building-1 building-2] 0)]
        :when (and (not= building-1 building-2) (> distance 0))
        day (range num-days)
        hour (range hours-per-day)
        [nd nh] (next-times-at-day [day hour] hours-per-day distance)]
    #{(negate (teacher-in-building teacher [day hour] building-1))
      (negate (teaches-at teacher [day hour]))
      (negate (teacher-in-building teacher [nd nh] building-2))}))

;; Beachten\ von\ Wechselzeiten\ zwischen\ Gebäuden:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def room-scheduling-constraint-functions
  [#'occurs-at-day-occurs-at-hour-relations
   #'occurs-at-hour-teached-at-relations
   #'class-clash-constraints
   #'teacher-clash-constraints
   #'everything-scheduled-constraints
   #'predetermined-room-constraints
   #'predetermined-times-constraints
   #'forbidden-rooms-constraints
   #'scheduled-rooms-not-available-constraints
   #'starts-at-hour-occurs-at-hour-relations
   #'illegal-starting-times-constraints
   #'occurs-at-hour-teaches-at-relations
   #'occurs-at-room-at-hour-occurs-at-room-relations
   #'everything-exactly-one-room-constraints
   #'scheduled-room-clash-constraints
   #'scheduled-rooms-class-in-buildings-relations
   #'scheduled-rooms-teacher-in-buildings-relations
   #'building-changes-constraints-for-classes
   #'building-changes-constraints-for-teachers])


(defn solve-problem-with-room-scheduling [entities]
  (solve-problem entities
                 {:constraint-functions room-scheduling-constraint-functions
                  :maxsat? false}))

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def room-scheuling-example
  {:classes {:class-1 {}}
   :teachers {:teacher-1 {}}
   :buildings {:building-1 {:room-1 {:times-not-available #{[0 0]}}
                            :room-2 {:times-not-available #{[0 1]}}}}
   :num-days 1
   :hours-per-day 2
   :events
   {
    :class-1-math
    (create-event :classes :class-1 :teachers #{:teacher-1} :number-of-times 2)}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def bigger-example-room-schedule-entities
  (merge (select-keys bigger-example-entities
                      [:classes :teachers :buildings :distances
                       :num-days :hours-per-day])
         {:events
          {:class-1-math
           (create-event :classes :class-1  :teachers #{:teacher-1} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]}) ;;hier auf 30 ändern
           :class-2-math
           (create-event :classes :class-2  :teachers #{:teacher-2} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-3-math
           (create-event :classes :class-3  :teachers #{:teacher-3} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]}) ;;hier auf 30 ändern
           :class-4-math
           (create-event :classes :class-4  :teachers #{:teacher-4} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-5-math
           (create-event :classes :class-5  :teachers #{:teacher-5} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]}) ;;hier auf 30 ändern
           :class-6-math
           (create-event :classes :class-6  :teachers #{:teacher-6} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]}):class-7-math
           (create-event :classes :class-7  :teachers #{:teacher-7} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]}) ;;hier auf 30 ändern
           :class-8-math
           (create-event :classes :class-8  :teachers #{:teacher-8} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-9-math
           (create-event :classes :class-9  :teachers #{:teacher-9} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]}) ;;hier auf 30 ändern
           :class-10-math
           (create-event :classes :class-10  :teachers #{:teacher-10} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]}):class-11-math
           (create-event :classes :class-11  :teachers #{:teacher-11} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]}) ;;hier auf 30 ändern
           :class-12-math
           (create-event :classes :class-12  :teachers #{:teacher-12} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-1-sk 
           (create-event :classes :class-1  :teachers #{:teacher-1} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-2-sk
           (create-event :classes :class-2  :teachers #{:teacher-2} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-3-sk 
           (create-event :classes :class-3  :teachers #{:teacher-3} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-4-sk
           (create-event :classes :class-4  :teachers #{:teacher-4} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-5-sk
           (create-event :classes :class-5  :teachers #{:teacher-5} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]} )
           :class-6-sk
           (create-event :classes :class-6  :teachers #{:teacher-6} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-7-sk 
           (create-event :classes :class-7  :teachers #{:teacher-7} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-8-sk
           (create-event :classes :class-8  :teachers #{:teacher-8} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-9-sk 
           (create-event :classes :class-9  :teachers #{:teacher-9} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-10-sk
           (create-event :classes :class-10  :teachers #{:teacher-10} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-11-sk 
           (create-event :classes :class-11  :teachers #{:teacher-11} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-12-sk
           (create-event :classes :class-12  :teachers #{:teacher-12} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})
           :class-1-german
           (create-event :classes :class-1  :teachers #{:teacher-1} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-2-german
           (create-event :classes :class-2  :teachers #{:teacher-2} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-3-german
           (create-event :classes :class-3  :teachers #{:teacher-3} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-4-german
           (create-event :classes :class-4  :teachers #{:teacher-4} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-5-german
           (create-event :classes :class-5  :teachers #{:teacher-5} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-6-german
           (create-event :classes :class-6  :teachers #{:teacher-6} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-7-german
           (create-event :classes :class-7  :teachers #{:teacher-7} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-8-german
           (create-event :classes :class-8  :teachers #{:teacher-8} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-9-german
           (create-event :classes :class-9  :teachers #{:teacher-9} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-10-german
           (create-event :classes :class-10  :teachers #{:teacher-10} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-11-german
           (create-event :classes :class-11  :teachers #{:teacher-11} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-12-german
           (create-event :classes :class-12  :teachers #{:teacher-12} :number-of-times 4 :forbidden-rooms #{[:building-2 :werkraum] [:building-1 :art-room] [:sporthalle :sporthalle]})         
           :class-1-sport
           (create-event :classes :class-1 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1 )         
           :class-2-sport
           (create-event :classes :class-2 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-3-sport
           (create-event :classes :class-3 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-4-sport
           (create-event :classes :class-4 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-5-sport
           (create-event :classes :class-5 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-6-sport
           (create-event :classes :class-6 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-7-sport
           (create-event :classes :class-7 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-8-sport
           (create-event :classes :class-8 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-9-sport
           (create-event :classes :class-9 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-10-sport
           (create-event :classes :class-10 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-11-sport
           (create-event :classes :class-11 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-12-sport
           (create-event :classes :class-12 :rooms #{[:sporthalle :sporthalle]} :teachers #{:sportlehrer} :durations [2] :number-of-times 1)         
           :class-1-art
           (create-event :classes :class-1 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-1} :number-of-times 2)         
           :class-2-art
           (create-event :classes :class-2 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-2} :number-of-times 2)         
           :class-3-art
           (create-event :classes :class-3 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-3} :number-of-times 2)         
           :class-4-art
           (create-event :classes :class-4 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-4} :number-of-times 2)         
           :class-5-art
           (create-event :classes :class-5 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-5} :number-of-times 2)         
           :class-6-art
           (create-event :classes :class-6 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-6} :number-of-times 2)         
           :class-7-art
           (create-event :classes :class-7 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-7} :number-of-times 2)         
           :class-8-art
           (create-event :classes :class-8 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-8} :number-of-times 2)         
           :class-9-art
           (create-event :classes :class-9 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-9} :number-of-times 2)         
           :class-10-art
           (create-event :classes :class-10 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-10} :number-of-times 2)         
           :class-11-art
           (create-event :classes :class-11 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-11} :number-of-times 2)         
           :class-12-art
           (create-event :classes :class-12 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-12} :number-of-times 2)         
           :class-1-werken
           (create-event :classes :class-1 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-1} :number-of-times 2)        
           :class-2-werken
           (create-event :classes :class-2 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-2} :number-of-times 2)         
           :class-3-werken
           (create-event :classes :class-3 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-3} :number-of-times 2)        
           :class-4-werken
           (create-event :classes :class-4 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-4} :number-of-times 2)         
           :class-5-werken
           (create-event :classes :class-5 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-5} :number-of-times 2)        
           :class-6-werken
           (create-event :classes :class-6 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-6} :number-of-times 2)         
           :class-7-werken
           (create-event :classes :class-7 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-7} :number-of-times 2)        
           :class-8-werken
           (create-event :classes :class-8 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-8} :number-of-times 2)         
           :class-9-werken
           (create-event :classes :class-9 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-9} :number-of-times 2)        
           :class-10-werken
           (create-event :classes :class-10 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-10} :number-of-times 2)         
           :class-11-werken
           (create-event :classes :class-11 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-11} :number-of-times 2)        
           :class-12-werken
           (create-event :classes :class-12 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-12} :number-of-times 2)}}))

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def example-result
  {:events {:class-9-werken {:classes #{:class-9}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-9} #{:teacher-9}], :durations [1 1], :forbidden-rooms #{}, :times [[2 5] [1 2]]}, :class-2-werken {:classes #{:class-2}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-2} #{:teacher-2}], :durations [1 1], :forbidden-rooms #{}, :times [[2 3] [3 0]]}, :class-10-sport {:classes #{:class-10}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[2 4]]}, :class-5-art {:classes #{:class-5}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-5} #{:teacher-5}], :durations [1 1], :forbidden-rooms #{}, :times [[0 3] [3 1]]}, :class-3-german {:classes #{:class-3}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-6]} #{[:building-2 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 1] [3 3] [4 1] [0 4]]}, :class-7-werken {:classes #{:class-7}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-7} #{:teacher-7}], :durations [1 1], :forbidden-rooms #{}, :times [[4 3] [4 2]]}, :class-3-math {:classes #{:class-3}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-1]} #{[:building-2 :room-2]} #{[:building-2 :room-1]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 0] [4 3] [4 2] [0 2]]}, :class-2-art {:classes #{:class-2}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-2} #{:teacher-2}], :durations [1 1], :forbidden-rooms #{}, :times [[3 4] [0 1]]}, :class-3-werken {:classes #{:class-3}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-3} #{:teacher-3}], :durations [1 1], :forbidden-rooms #{}, :times [[1 4] [3 1]]}, :class-1-sk {:classes #{:class-1}, :rooms [#{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-1 :room-1]} #{[:building-2 :room-6]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 0] [2 1] [0 4] [2 4]]}, :class-8-math {:classes #{:class-8}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-1 :room-6]} #{[:building-2 :room-6]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 4] [2 3] [4 1] [4 5]]}, :class-1-art {:classes #{:class-1}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-1} #{:teacher-1}], :durations [1 1], :forbidden-rooms #{}, :times [[4 3] [2 0]]}, :class-6-werken {:classes #{:class-6}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-6} #{:teacher-6}], :durations [1 1], :forbidden-rooms #{}, :times [[2 2] [3 5]]}, :class-10-art {:classes #{:class-10}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-10} #{:teacher-10}], :durations [1 1], :forbidden-rooms #{}, :times [[4 4] [3 5]]}, :class-9-sport {:classes #{:class-9}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[3 2]]}, :class-12-art {:classes #{:class-12}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-12} #{:teacher-12}], :durations [1 1], :forbidden-rooms #{}, :times [[2 1] [1 2]]}, :class-1-sport {:classes #{:class-1}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[3 0]]}, :class-9-art {:classes #{:class-9}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-9} #{:teacher-9}], :durations [1 1], :forbidden-rooms #{}, :times [[2 2] [4 0]]}, :class-10-werken {:classes #{:class-10}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-10} #{:teacher-10}], :durations [1 1], :forbidden-rooms #{}, :times [[1 5] [1 0]]}, :class-8-sport {:classes #{:class-8}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[0 2]]}, :class-3-sk {:classes #{:class-3}, :rooms [#{[:building-1 :room-3]} #{[:building-1 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-1]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 5] [1 1] [3 0] [4 5]]}, :class-11-werken {:classes #{:class-11}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-11} #{:teacher-11}], :durations [1 1], :forbidden-rooms #{}, :times [[4 1] [4 4]]}, :class-7-math {:classes #{:class-7}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-4]} #{[:building-2 :room-4]} #{[:building-1 :room-6]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 1] [2 3] [2 2] [1 0]]}, :class-8-art {:classes #{:class-8}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-8} #{:teacher-8}], :durations [1 1], :forbidden-rooms #{}, :times [[4 2] [0 5]]}, :class-4-art {:classes #{:class-4}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-4} #{:teacher-4}], :durations [1 1], :forbidden-rooms #{}, :times [[1 4] [2 5]]}, :class-7-german {:classes #{:class-7}, :rooms [#{[:building-1 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-6]} #{[:building-1 :room-6]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 5] [2 0] [3 1] [0 1]]}, :class-6-sport {:classes #{:class-6}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[4 4]]}, :class-11-sport {:classes #{:class-11}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[3 4]]}, :class-2-german {:classes #{:class-2}, :rooms [#{[:building-2 :room-3]} #{[:building-1 :room-1]} #{[:building-2 :room-4]} #{[:building-1 :room-4]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 3] [0 0] [4 3] [4 0]]}, :class-7-art {:classes #{:class-7}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-7} #{:teacher-7}], :durations [1 1], :forbidden-rooms #{}, :times [[1 5] [4 5]]}, :class-4-math {:classes #{:class-4}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-2]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 0] [0 3] [0 0] [0 5]]}, :class-6-german {:classes #{:class-6}, :rooms [#{[:building-2 :room-5]} #{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-2]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 5] [3 2] [1 2] [3 1]]}, :class-4-german {:classes #{:class-4}, :rooms [#{[:building-2 :room-3]} #{[:building-2 :room-5]} #{[:building-2 :room-1]} #{[:building-2 :room-6]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 3] [0 4] [2 3] [3 2]]}, :class-1-german {:classes #{:class-1}, :rooms [#{[:building-2 :room-1]} #{[:building-1 :room-6]} #{[:building-2 :room-5]} #{[:building-2 :room-4]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 3] [4 2] [4 5] [1 5]]}, :class-11-math {:classes #{:class-11}, :rooms [#{[:building-2 :room-6]} #{[:building-1 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-1]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 4] [1 3] [0 2] [3 1]]}, :class-11-art {:classes #{:class-11}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-11} #{:teacher-11}], :durations [1 1], :forbidden-rooms #{}, :times [[2 3] [1 1]]}, :class-2-math {:classes #{:class-2}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-2]} #{[:building-1 :room-1]} #{[:building-2 :room-2]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 2] [0 3] [1 5] [2 2]]}, :class-12-werken {:classes #{:class-12}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-12} #{:teacher-12}], :durations [1 1], :forbidden-rooms #{}, :times [[4 0] [3 3]]}, :class-6-sk {:classes #{:class-6}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-5]} #{[:building-2 :room-5]} #{[:building-2 :room-5]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 4] [2 0] [4 2] [2 4]]}, :class-4-sport {:classes #{:class-4}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[4 0]]}, :class-11-sk {:classes #{:class-11}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-1]} #{[:building-2 :room-6]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 5] [2 5] [4 0] [3 0]]}, :class-2-sport {:classes #{:class-2}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[1 0]]}, :class-2-sk {:classes #{:class-2}, :rooms [#{[:building-1 :room-5]} #{[:building-2 :room-1]} #{[:building-2 :room-6]} #{[:building-2 :room-1]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 2] [2 5] [4 4] [0 5]]}, :class-8-werken {:classes #{:class-8}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-8} #{:teacher-8}], :durations [1 1], :forbidden-rooms #{}, :times [[3 2] [2 1]]}, :class-5-werken {:classes #{:class-5}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-5} #{:teacher-5}], :durations [1 1], :forbidden-rooms #{}, :times [[1 3] [3 4]]}, :class-12-sk {:classes #{:class-12}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-3]} #{[:building-1 :room-5]} #{[:building-1 :room-2]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 4] [3 5] [0 4] [2 5]]}, :class-3-sport {:classes #{:class-3}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[2 0]]}, :class-7-sport {:classes #{:class-7}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[0 4]]}, :class-12-math {:classes #{:class-12}, :rooms [#{[:building-1 :room-6]} #{[:building-2 :room-6]} #{[:building-1 :room-6]} #{[:building-2 :room-2]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 5] [4 1] [0 3] [4 3]]}, :class-4-werken {:classes #{:class-4}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-4} #{:teacher-4}], :durations [1 1], :forbidden-rooms #{}, :times [[0 2] [2 0]]}, :class-10-math {:classes #{:class-10}, :rooms [#{[:building-1 :room-3]} #{[:building-1 :room-3]} #{[:building-2 :room-4]} #{[:building-1 :room-6]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 2] [2 1] [1 4] [3 4]]}, :class-5-sk {:classes #{:class-5}, :rooms [#{[:building-2 :room-5]} #{[:building-2 :room-2]} #{[:building-2 :room-6]} #{[:building-2 :room-2]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 0] [1 4] [2 1] [3 3]]}, :class-9-german {:classes #{:class-9}, :rooms [#{[:building-2 :room-1]} #{[:building-1 :room-1]} #{[:building-1 :room-1]} #{[:building-2 :room-6]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 5] [4 1] [0 1] [2 0]]}, :class-3-art {:classes #{:class-3}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-3} #{:teacher-3}], :durations [1 1], :forbidden-rooms #{}, :times [[2 4] [1 0]]}, :class-9-sk {:classes #{:class-9}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-1 :room-3]} #{[:building-2 :room-1]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 4] [1 1] [0 3] [1 5]]}, :class-11-german {:classes #{:class-11}, :rooms [#{[:building-1 :room-6]} #{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 2] [2 0] [3 2] [4 2]]}, :class-4-sk {:classes #{:class-4}, :rooms [#{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-2 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 2] [3 5] [3 3] [4 4]]}, :class-5-sport {:classes #{:class-5}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[0 0]]}, :class-6-art {:classes #{:class-6}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-6} #{:teacher-6}], :durations [1 1], :forbidden-rooms #{}, :times [[0 0] [0 4]]}, :class-9-math {:classes #{:class-9}, :rooms [#{[:building-2 :room-5]} #{[:building-1 :room-6]} #{[:building-1 :room-6]} #{[:building-2 :room-1]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 3] [0 0] [2 3] [3 0]]}, :class-8-german {:classes #{:class-8}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-4]} #{[:building-2 :room-1]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 0] [1 2] [4 4] [2 4]]}, :class-1-werken {:classes #{:class-1}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-1} #{:teacher-1}], :durations [1 1], :forbidden-rooms #{}, :times [[0 1] [1 1]]}, :class-5-math {:classes #{:class-5}, :rooms [#{[:building-1 :room-2]} #{[:building-1 :room-6]} #{[:building-2 :room-6]} #{[:building-1 :room-6]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 0] [1 1] [2 2] [0 4]]}, :class-10-german {:classes #{:class-10}, :rooms [#{[:building-1 :room-4]} #{[:building-2 :room-5]} #{[:building-1 :room-1]} #{[:building-1 :room-5]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 2] [3 2] [0 2] [4 0]]}, :class-7-sk {:classes #{:class-7}, :rooms [#{[:building-2 :room-4]} #{[:building-2 :room-4]} #{[:building-1 :room-2]} #{[:building-2 :room-4]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 5] [3 3] [0 0] [3 2]]}, :class-1-math {:classes #{:class-1}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-4]} #{[:building-1 :room-4]} #{[:building-1 :room-5]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 2] [1 0] [2 2] [0 3]]}, :class-12-german {:classes #{:class-12}, :rooms [#{[:building-1 :room-3]} #{[:building-2 :room-2]} #{[:building-2 :room-3]} #{[:building-1 :room-5]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 2] [0 0] [4 2] [1 1]]}, :class-6-math {:classes #{:class-6}, :rooms [#{[:building-1 :room-1]} #{[:building-2 :room-2]} #{[:building-2 :room-2]} #{[:building-2 :room-1]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 1] [4 0] [0 2] [2 1]]}, :class-5-german {:classes #{:class-5}, :rooms [#{[:building-1 :room-6]} #{[:building-2 :room-5]} #{[:building-2 :room-6]} #{[:building-2 :room-4]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 4] [4 1] [3 5] [4 2]]}, :class-8-sk {:classes #{:class-8}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-6]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 1] [2 2] [1 4] [1 3]]}, :class-12-sport {:classes #{:class-12}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[1 4]]}, :class-10-sk {:classes #{:class-10}, :rooms [#{[:building-1 :room-2]} #{[:building-1 :room-3]} #{[:building-1 :room-5]} #{[:building-1 :room-2]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 3] [1 2] [0 0] [2 0]]}}, :hours-per-day 6, :num-days 5, :distances {[:building-1 :sporthalle] 1, [:sporthalle :building-2] 1, [:sporthalle :building-1] 1, [:building-2 :sporthalle] 1, [:building-2 :building-1] 1, [:building-1 :building-2] 1}, :buildings {:building-1 {:room-1 {}, :room-2 {}, :room-3 {}, :room-4 {}, :room-5 {}, :room-6 {}, :art-room {}}, :building-2 {:room-1 {}, :room-2 {}, :room-3 {}, :room-4 {}, :room-5 {}, :room-6 {}, :werkraum {}}, :sporthalle {:sporthalle {}}}, :teachers {:teacher-7 {}, :teacher-6 {}, :teacher-10 {}, :teacher-1 {}, :teacher-11 {}, :sportlehrer {}, :teacher-4 {}, :teacher-2 {}, :teacher-12 {}, :teacher-8 {}, :teacher-3 {}, :teacher-5 {}, :teacher-9 {}}, :classes {:class-4 {:class-teacher :teacher-4, :class-room [:building-2 :room-2]}, :class-10 {:class-teacher :teacher-10, :class-room [:building-2 :room-5]}, :class-11 {:class-teacher :teacher-11, :class-room [:building-1 :room-6]}, :class-8 {:class-teacher :teacher-8, :class-room [:building-2 :room-4]}, :class-2 {:class-teacher :teacher-2, :class-room [:building-2 :room-1]}, :class-12 {:class-teacher :teacher-12, :class-room [:building-2 :room-6]}, :class-3 {:class-teacher :teacher-3, :class-room [:building-1 :room-2]}, :class-5 {:class-teacher :teacher-5, :class-room [:building-1 :room-3]}, :class-1 {:class-teacher :teacher-1, :class-room [:building-1 :room-1]}, :class-7 {:class-teacher :teacher-7, :class-room [:building-1 :room-4]}, :class-6 {:class-teacher :teacher-6, :class-room [:building-2 :room-3]}, :class-9 {:class-teacher :teacher-9, :class-room [:building-1 :room-5]}}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Untersuchung%20der%20Skalierbarkeit][Untersuchung\ der\ Skalierbarkeit:1]]

(defn create-scalable-room-scheduling-problem [num-hours num-days num-classes]
  (let [num-slots (* num-days num-hours)]
    {:classes (reduce #(assoc %1 (keyword (str "class-" %2))
                              {:class-teacher (keyword (str "teacher-" %2))
                               :class-room [(keyword (str "building-" %2)) :room-1]})
                      {} (range num-classes))
     :teachers (reduce #(assoc %1 (keyword (str "teacher-" %2)) {}) {} (range num-classes))
     :distances {} 
     :num-days num-days
     :hours-per-day num-hours
     :buildings (reduce #(assoc %1 (keyword (str "building-" %2)) {:room-1 {}})
                        {} (range num-classes))
     :events (reduce #(assoc %1 (keyword (str "ef-" %2))
                             (create-event :classes (keyword (str "class-" %2))
                                           :teachers #{(keyword (str "teacher-" %2))}
                                           :number-of-times num-slots))
                     {} (range num-classes))}))

;; Untersuchung\ der\ Skalierbarkeit:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Untersuchung%20der%20Skalierbarkeit][Untersuchung\ der\ Skalierbarkeit:1]]

(defn create-scalable-room-scheduling-problem-one-building [num-hours num-days num-classes]
  (let [num-slots (* num-days num-hours)]
    {:classes (reduce #(assoc %1 (keyword (str "class-" %2))
                              {:class-teacher (keyword (str "teacher-" %2))
                               :class-room [:b-1 (keyword (str "room-" %2))]})
                      {} (range num-classes))
     :teachers (reduce #(assoc %1 (keyword (str "teacher-" %2)) {}) {} (range num-classes))
     :distances {}
     :num-days num-days
     :hours-per-day num-hours
     :buildings (reduce #(assoc %1 (keyword (str "building-" %2)) {:room-1 {}})
                        {} (range num-classes))
     :events (reduce #(assoc %1 (keyword (str "ef-" %2))
                             (create-event :classes (keyword (str "class-" %2))
                                           :teachers #{(keyword (str "teacher-" %2))}
                                           :number-of-times num-slots))
                     {} (range num-classes))}))

;; Untersuchung\ der\ Skalierbarkeit:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Untersuchung%20der%20Skalierbarkeit][Untersuchung\ der\ Skalierbarkeit:1]]

(defn bigger-example-room-scheduling-entities-more-rooms [roomcount]
  (-> bigger-example-room-schedule-entities
      (update-in [:buildings] assoc :new-building
                 (into {} (for [i (range roomcount)] [(keyword (str "room-" i)) {}])))
      (update-in [:distances]
                 (fn [distances]
                   (reduce #(assoc %1 %2 1)
                           distances
                           (for [b (:buildings bigger-example-room-schedule-entities)
                                 key [[b :new-building] [:new-building b]]]
                             key))))))

;; Untersuchung\ der\ Skalierbarkeit:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Optimal%20room%20scheduling%20constraints][Optimal\ room\ scheduling\ constraints:1]]

(def optimal-constraints-for-room-scheduling
  [#'occurs-at-day-occurs-at-hour-relations
   #'occurs-at-hour-teached-at-relations
   #'class-clash-constraints
   #'teacher-clash-constraints
   #'everything-scheduled-constraints
   #'predetermined-room-constraints
   #'predetermined-times-constraints
   #'forbidden-rooms-constraints
   #'scheduled-rooms-not-available-constraints
   #'starts-at-hour-occurs-at-hour-relations
   #'illegal-starting-times-constraints
   #'occurs-at-hour-teaches-at-relations
   #'occurs-at-room-at-hour-occurs-at-room-relations
   #'everything-exactly-one-room-constraints
   #'scheduled-room-clash-constraints
   #'minimize-gaps-for-classes-constraints
   #'minimize-gaps-for-teachers-constraints
   #'scheduled-rooms-class-in-buildings-relations
   #'scheduled-rooms-teacher-in-buildings-relations
   #'minimize-building-changes-constraints-for-classes
   #'minimize-building-changes-constraints-for-teachers
   #'building-changes-constraints-for-classes
   #'building-changes-constraints-for-teachers])


(defn solve-problem-optimally-with-room-scheduling [entities]
  (solve-problem entities {:constraint-functions optimal-constraints-for-room-scheduling :maxsat? true}))

;; Optimal\ room\ scheduling\ constraints:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Neues%20Beispielprogramm][Neues\ Beispielprogramm:1]]

(def example-room-schedule-entities
  (merge (select-keys example-entities
                      [:classes :buildings :num-days :hours-per-day :teachers :distances])
         {:events
          {:class-1-math
           (create-event :classes :class-1  :teachers #{:teacher-1} :number-of-times 4)
           :class-2-math
           (create-event :classes :class-2  :teachers #{:teacher-2} :number-of-times 4)
           :class-1-sk 
           (create-event :classes :class-1  :teachers #{:teacher-1} :number-of-times 4)
           :class-2-sk
           (create-event :classes :class-2  :teachers #{:teacher-2} :number-of-times 4)
           :class-1-german
           (create-event :classes :class-1  :teachers #{:teacher-1} :number-of-times 4)         
           :class-2-german
           (create-event :classes :class-2  :teachers #{:teacher-2} :number-of-times 4)
           :class-1-sport
           (create-event :classes :class-1 :rooms #{[:sporthalle :sporthalle]} :teachers #{:teacher-2} :durations [2] :number-of-times 1)         
           :class-2-sport
           (create-event :classes :class-2 :rooms #{[:sporthalle :sporthalle]} :teachers #{:teacher-2} :durations [2] :number-of-times 1)
           :class-1-art
           (create-event :classes :class-1 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-1} :number-of-times 4)         
           :class-2-art
           (create-event :classes :class-2 :rooms #{[:building-1 :art-room]} :teachers #{:teacher-1} :number-of-times 4)
           :class-1-werken
           (create-event :classes :class-1 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-1} :number-of-times 2)        
           :class-2-werken
           (create-event :classes :class-2 :rooms #{[:building-2 :werkraum]} :teachers #{:teacher-1} :number-of-times 2) }}))

;; Neues\ Beispielprogramm:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def optimal-room-scheduling-example
  {
   :classes {:class-1 {}
             :class-2 {}}
   :teachers {:teacher-1 {}}
   :buildings {:building-1 {:room-1 {}}
               :building-2 {:room-1 {}}}
   :distances {[:building-1 :building-2] 1
               [:building-2 :building-1] 1}
   :num-days 1
   :hours-per-day 3
   :events {
            :class-1-math
            (create-event :classes :class-1 :teachers #{:teacher-1} :number-of-times 2)
            :class-2-math
            (create-event :classes :class-2 :teachers #{:teacher-2} :number-of-times 2)}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def example-ents-optimal-room-scheduling-without-optimisation
{:classes {:class-1 {}, :class-2 {}}, :teachers {:teacher-1 {}}, :buildings {:building-1 {:room-1 {}}, :building-2 {:room-1 {}}}, :distances {[:building-2 :building-1] 1, [:building-1 :building-2] 1}, :num-days 1, :hours-per-day 3, :events {:class-1-math {:classes #{:class-1}, :rooms [#{[:building-2 :room-1]} #{[:building-1 :room-1]}], :teachers [#{:teacher-1} #{:teacher-1}], :durations [1 1], :forbidden-rooms #{}, :times [[0 2] [0 0]]}, :class-2-math {:classes #{:class-2}, :rooms [#{[:building-1 :room-1]} #{[:building-1 :room-1]}], :teachers [#{:teacher-2} #{:teacher-2}], :durations [1 1], :forbidden-rooms #{}, :times [[0 2] [0 1]]}}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def example-ents-optimal-room-scheduling-with-optimisation
  {:classes {:class-1 {}, :class-2 {}}, :teachers {:teacher-1 {}}, :buildings {:building-1 {:room-1 {}}, :building-2 {:room-1 {}}}, :distances {[:building-2 :building-1] 1, [:building-1 :building-2] 1}, :num-days 1, :hours-per-day 3, :events {:class-1-math {:classes #{:class-1}, :rooms [#{[:building-1 :room-1]} #{[:building-1 :room-1]}], :teachers [#{:teacher-1} #{:teacher-1}], :durations [1 1], :forbidden-rooms #{}, :times [[0 2] [0 1]]}, :class-2-math {:classes #{:class-2}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-2} #{:teacher-2}], :durations [1 1], :forbidden-rooms #{}, :times [[0 1] [0 2]]}}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Ergebnisse][Ergebnisse:1]]

(def optimal-room-scheduling-bigger-example-entities-results
  {:events {:class-9-werken {:classes #{:class-9}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-9} #{:teacher-9}], :durations [1 1], :forbidden-rooms #{}, :times [[4 5] [0 1]]}, :class-2-werken {:classes #{:class-2}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-2} #{:teacher-2}], :durations [1 1], :forbidden-rooms #{}, :times [[4 2] [3 0]]}, :class-10-sport {:classes #{:class-10}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[4 0]]}, :class-5-art {:classes #{:class-5}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-5} #{:teacher-5}], :durations [1 1], :forbidden-rooms #{}, :times [[0 2] [0 1]]}, :class-3-german {:classes #{:class-3}, :rooms [#{[:building-2 :room-3]} #{[:building-2 :room-6]} #{[:building-1 :room-3]} #{[:building-2 :room-6]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 1] [1 3] [2 5] [0 2]]}, :class-7-werken {:classes #{:class-7}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-7} #{:teacher-7}], :durations [1 1], :forbidden-rooms #{}, :times [[1 0] [4 4]]}, :class-3-math {:classes #{:class-3}, :rooms [#{[:building-2 :room-3]} #{[:building-1 :room-5]} #{[:building-1 :room-4]} #{[:building-2 :room-2]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 0] [2 2] [4 0] [1 4]]}, :class-2-art {:classes #{:class-2}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-2} #{:teacher-2}], :durations [1 1], :forbidden-rooms #{}, :times [[1 2] [1 5]]}, :class-3-werken {:classes #{:class-3}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-3} #{:teacher-3}], :durations [1 1], :forbidden-rooms #{}, :times [[0 0] [0 3]]}, :class-1-sk {:classes #{:class-1}, :rooms [#{[:building-1 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-6]} #{[:building-2 :room-2]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 0] [4 0] [4 4] [4 3]]}, :class-8-math {:classes #{:class-8}, :rooms [#{[:building-2 :room-6]} #{[:building-1 :room-4]} #{[:building-2 :room-1]} #{[:building-1 :room-4]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 2] [2 2] [4 4] [2 4]]}, :class-1-art {:classes #{:class-1}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-1} #{:teacher-1}], :durations [1 1], :forbidden-rooms #{}, :times [[1 1] [1 3]]}, :class-6-werken {:classes #{:class-6}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-6} #{:teacher-6}], :durations [1 1], :forbidden-rooms #{}, :times [[0 5] [0 2]]}, :class-10-art {:classes #{:class-10}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-10} #{:teacher-10}], :durations [1 1], :forbidden-rooms #{}, :times [[0 4] [0 3]]}, :class-9-sport {:classes #{:class-9}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[2 0]]}, :class-12-art {:classes #{:class-12}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-12} #{:teacher-12}], :durations [1 1], :forbidden-rooms #{}, :times [[0 0] [1 0]]}, :class-1-sport {:classes #{:class-1}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[2 4]]}, :class-9-art {:classes #{:class-9}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-9} #{:teacher-9}], :durations [1 1], :forbidden-rooms #{}, :times [[3 1] [3 3]]}, :class-10-werken {:classes #{:class-10}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-10} #{:teacher-10}], :durations [1 1], :forbidden-rooms #{}, :times [[1 3] [1 4]]}, :class-8-sport {:classes #{:class-8}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[0 2]]}, :class-3-sk {:classes #{:class-3}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-3]} #{[:building-2 :room-2]} #{[:building-1 :room-6]}], :teachers [#{:teacher-3} #{:teacher-3} #{:teacher-3} #{:teacher-3}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 1] [0 5] [1 2] [4 3]]}, :class-11-werken {:classes #{:class-11}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-11} #{:teacher-11}], :durations [1 1], :forbidden-rooms #{}, :times [[3 5] [1 2]]}, :class-7-math {:classes #{:class-7}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-1]} #{[:building-1 :room-3]} #{[:building-2 :room-6]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 3] [4 3] [2 4] [1 5]]}, :class-8-art {:classes #{:class-8}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-8} #{:teacher-8}], :durations [1 1], :forbidden-rooms #{}, :times [[2 3] [3 5]]}, :class-4-art {:classes #{:class-4}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-4} #{:teacher-4}], :durations [1 1], :forbidden-rooms #{}, :times [[3 2] [3 0]]}, :class-7-german {:classes #{:class-7}, :rooms [#{[:building-2 :room-1]} #{[:building-1 :room-3]} #{[:building-2 :room-5]} #{[:building-2 :room-4]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 0] [2 0] [4 1] [4 5]]}, :class-6-sport {:classes #{:class-6}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[1 4]]}, :class-11-sport {:classes #{:class-11}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[4 4]]}, :class-2-german {:classes #{:class-2}, :rooms [#{[:building-1 :room-1]} #{[:building-2 :room-3]} #{[:building-1 :room-5]} #{[:building-1 :room-2]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 0] [3 5] [1 4] [2 4]]}, :class-7-art {:classes #{:class-7}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-7} #{:teacher-7}], :durations [1 1], :forbidden-rooms #{}, :times [[3 4] [2 5]]}, :class-4-math {:classes #{:class-4}, :rooms [#{[:building-1 :room-6]} #{[:building-1 :room-5]} #{[:building-2 :room-3]} #{[:building-1 :room-2]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 4] [2 3] [4 4] [3 3]]}, :class-6-german {:classes #{:class-6}, :rooms [#{[:building-2 :room-6]} #{[:building-1 :room-3]} #{[:building-1 :room-4]} #{[:building-1 :room-3]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 3] [3 4] [4 2] [3 5]]}, :class-4-german {:classes #{:class-4}, :rooms [#{[:building-1 :room-5]} #{[:building-1 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-4]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 1] [2 2] [4 5] [0 5]]}, :class-1-german {:classes #{:class-1}, :rooms [#{[:building-1 :room-3]} #{[:building-1 :room-3]} #{[:building-2 :room-1]} #{[:building-1 :room-3]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 5] [1 4] [3 1] [0 4]]}, :class-11-math {:classes #{:class-11}, :rooms [#{[:building-1 :room-1]} #{[:building-1 :room-6]} #{[:building-2 :room-1]} #{[:building-2 :room-6]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 4] [2 3] [1 5] [3 4]]}, :class-11-art {:classes #{:class-11}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-11} #{:teacher-11}], :durations [1 1], :forbidden-rooms #{}, :times [[2 0] [2 2]]}, :class-2-math {:classes #{:class-2}, :rooms [#{[:building-2 :room-6]} #{[:building-1 :room-2]} #{[:building-2 :room-1]} #{[:building-2 :room-3]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 1] [1 3] [3 4] [4 5]]}, :class-12-werken {:classes #{:class-12}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-12} #{:teacher-12}], :durations [1 1], :forbidden-rooms #{}, :times [[3 1] [2 5]]}, :class-6-sk {:classes #{:class-6}, :rooms [#{[:building-2 :room-6]} #{[:building-1 :room-3]} #{[:building-1 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 5] [4 0] [3 0] [0 0]]}, :class-4-sport {:classes #{:class-4}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[1 0]]}, :class-11-sk {:classes #{:class-11}, :rooms [#{[:building-2 :room-2]} #{[:building-2 :room-6]} #{[:building-2 :room-5]} #{[:building-1 :room-2]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 0] [3 3] [3 1] [2 1]]}, :class-2-sport {:classes #{:class-2}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[0 4]]}, :class-2-sk {:classes #{:class-2}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-2]} #{[:building-1 :room-1]} #{[:building-2 :room-1]}], :teachers [#{:teacher-2} #{:teacher-2} #{:teacher-2} #{:teacher-2}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 3] [4 4] [2 5] [3 2]]}, :class-8-werken {:classes #{:class-8}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-8} #{:teacher-8}], :durations [1 1], :forbidden-rooms #{}, :times [[4 3] [1 5]]}, :class-5-werken {:classes #{:class-5}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-5} #{:teacher-5}], :durations [1 1], :forbidden-rooms #{}, :times [[2 3] [2 2]]}, :class-12-sk {:classes #{:class-12}, :rooms [#{[:building-2 :room-6]} #{[:building-2 :room-1]} #{[:building-1 :room-3]} #{[:building-2 :room-5]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 4] [3 0] [0 1] [2 3]]}, :class-3-sport {:classes #{:class-3}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[3 4]]}, :class-7-sport {:classes #{:class-7}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[0 0]]}, :class-12-math {:classes #{:class-12}, :rooms [#{[:building-1 :room-3]} #{[:building-2 :room-1]} #{[:building-2 :room-2]} #{[:building-1 :room-5]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 3] [2 2] [3 4] [0 3]]}, :class-4-werken {:classes #{:class-4}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-4} #{:teacher-4}], :durations [1 1], :forbidden-rooms #{}, :times [[4 0] [0 4]]}, :class-10-math {:classes #{:class-10}, :rooms [#{[:building-1 :room-6]} #{[:building-1 :room-5]} #{[:building-2 :room-5]} #{[:building-2 :room-2]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 0] [2 4] [3 2] [3 5]]}, :class-5-sk {:classes #{:class-5}, :rooms [#{[:building-2 :room-4]} #{[:building-1 :room-2]} #{[:building-1 :room-5]} #{[:building-1 :room-6]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 4] [4 2] [0 5] [0 0]]}, :class-9-german {:classes #{:class-9}, :rooms [#{[:building-2 :room-5]} #{[:building-1 :room-2]} #{[:building-1 :room-3]} #{[:building-1 :room-3]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 0] [1 0] [3 0] [1 1]]}, :class-3-art {:classes #{:class-3}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-3} #{:teacher-3}], :durations [1 1], :forbidden-rooms #{}, :times [[4 2] [2 4]]}, :class-9-sk {:classes #{:class-9}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-4]} #{[:building-1 :room-6]} #{[:building-1 :room-1]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 5] [4 3] [1 4] [1 5]]}, :class-11-german {:classes #{:class-11}, :rooms [#{[:building-1 :room-6]} #{[:building-1 :room-4]} #{[:building-1 :room-6]} #{[:building-2 :room-6]}], :teachers [#{:teacher-11} #{:teacher-11} #{:teacher-11} #{:teacher-11}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[2 5] [0 3] [0 5] [3 2]]}, :class-4-sk {:classes #{:class-4}, :rooms [#{[:building-1 :room-2]} #{[:building-1 :room-2]} #{[:building-2 :room-1]} #{[:building-2 :room-4]}], :teachers [#{:teacher-4} #{:teacher-4} #{:teacher-4} #{:teacher-4}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 5] [3 4] [0 2] [4 2]]}, :class-5-sport {:classes #{:class-5}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[3 0]]}, :class-6-art {:classes #{:class-6}, :rooms [#{[:building-1 :art-room]} #{[:building-1 :art-room]}], :teachers [#{:teacher-6} #{:teacher-6}], :durations [1 1], :forbidden-rooms #{}, :times [[4 3] [4 5]]}, :class-9-math {:classes #{:class-9}, :rooms [#{[:building-2 :room-3]} #{[:building-2 :room-2]} #{[:building-2 :room-1]} #{[:building-2 :room-6]}], :teachers [#{:teacher-9} #{:teacher-9} #{:teacher-9} #{:teacher-9}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 0] [0 4] [4 2] [4 1]]}, :class-8-german {:classes #{:class-8}, :rooms [#{[:building-2 :room-5]} #{[:building-1 :room-4]} #{[:building-2 :room-3]} #{[:building-1 :room-5]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 5] [2 1] [1 1] [2 5]]}, :class-1-werken {:classes #{:class-1}, :rooms [#{[:building-2 :werkraum]} #{[:building-2 :werkraum]}], :teachers [#{:teacher-1} #{:teacher-1}], :durations [1 1], :forbidden-rooms #{}, :times [[3 3] [4 1]]}, :class-5-math {:classes #{:class-5}, :rooms [#{[:building-1 :room-6]} #{[:building-2 :room-5]} #{[:building-2 :room-2]} #{[:building-1 :room-6]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 4] [2 4] [1 5] [4 0]]}, :class-10-german {:classes #{:class-10}, :rooms [#{[:building-2 :room-1]} #{[:building-1 :room-4]} #{[:building-2 :room-4]} #{[:building-2 :room-4]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[3 3] [2 5] [1 5] [3 4]]}, :class-7-sk {:classes #{:class-7}, :rooms [#{[:building-2 :room-5]} #{[:building-2 :room-4]} #{[:building-1 :room-6]} #{[:building-2 :room-3]}], :teachers [#{:teacher-7} #{:teacher-7} #{:teacher-7} #{:teacher-7}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 2] [1 2] [2 1] [1 4]]}, :class-1-math {:classes #{:class-1}, :rooms [#{[:building-1 :room-4]} #{[:building-2 :room-1]} #{[:building-2 :room-5]} #{[:building-1 :room-6]}], :teachers [#{:teacher-1} #{:teacher-1} #{:teacher-1} #{:teacher-1}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 2] [3 5] [3 0] [1 5]]}, :class-12-german {:classes #{:class-12}, :rooms [#{[:building-1 :room-3]} #{[:building-1 :room-1]} #{[:building-2 :room-6]} #{[:building-2 :room-5]}], :teachers [#{:teacher-12} #{:teacher-12} #{:teacher-12} #{:teacher-12}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 5] [1 4] [2 0] [3 5]]}, :class-6-math {:classes #{:class-6}, :rooms [#{[:building-1 :room-4]} #{[:building-2 :room-4]} #{[:building-1 :room-1]} #{[:building-1 :room-6]}], :teachers [#{:teacher-6} #{:teacher-6} #{:teacher-6} #{:teacher-6}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[4 4] [0 4] [4 1] [3 1]]}, :class-5-german {:classes #{:class-5}, :rooms [#{[:building-1 :room-6]} #{[:building-2 :room-5]} #{[:building-2 :room-6]} #{[:building-1 :room-4]}], :teachers [#{:teacher-5} #{:teacher-5} #{:teacher-5} #{:teacher-5}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 3] [2 0] [2 1] [4 1]]}, :class-8-sk {:classes #{:class-8}, :rooms [#{[:building-2 :room-1]} #{[:building-2 :room-1]} #{[:building-2 :room-2]} #{[:building-2 :room-6]}], :teachers [#{:teacher-8} #{:teacher-8} #{:teacher-8} #{:teacher-8}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[1 3] [1 4] [1 0] [4 2]]}, :class-12-sport {:classes #{:class-12}, :rooms [#{[:sporthalle :sporthalle]}], :teachers [#{:sportlehrer}], :durations [2], :forbidden-rooms #{}, :times [[4 2]]}, :class-10-sk {:classes #{:class-10}, :rooms [#{[:building-1 :room-3]} #{[:building-1 :room-1]} #{[:building-2 :room-4]} #{[:building-1 :room-1]}], :teachers [#{:teacher-10} #{:teacher-10} #{:teacher-10} #{:teacher-10}], :durations [1 1 1 1], :forbidden-rooms #{[:building-1 :art-room] [:sporthalle :sporthalle] [:building-2 :werkraum]}, :times [[0 2] [0 5] [3 1] [2 3]]}}, :hours-per-day 6, :num-days 5, :distances {[:building-1 :sporthalle] 1, [:sporthalle :building-2] 1, [:sporthalle :building-1] 1, [:building-2 :sporthalle] 1, [:building-2 :building-1] 1, [:building-1 :building-2] 1}, :buildings {:building-1 {:room-1 {}, :room-2 {}, :room-3 {}, :room-4 {}, :room-5 {}, :room-6 {}, :art-room {}}, :building-2 {:room-1 {}, :room-2 {}, :room-3 {}, :room-4 {}, :room-5 {}, :room-6 {}, :werkraum {}}, :sporthalle {:sporthalle {}}}, :teachers {:teacher-7 {}, :teacher-6 {}, :teacher-10 {}, :teacher-1 {}, :teacher-11 {}, :sportlehrer {}, :teacher-4 {}, :teacher-2 {}, :teacher-12 {}, :teacher-8 {}, :teacher-3 {}, :teacher-5 {}, :teacher-9 {}}, :classes {:class-4 {:class-teacher :teacher-4, :class-room [:building-2 :room-2]}, :class-10 {:class-teacher :teacher-10, :class-room [:building-2 :room-5]}, :class-11 {:class-teacher :teacher-11, :class-room [:building-1 :room-6]}, :class-8 {:class-teacher :teacher-8, :class-room [:building-2 :room-4]}, :class-2 {:class-teacher :teacher-2, :class-room [:building-2 :room-1]}, :class-12 {:class-teacher :teacher-12, :class-room [:building-2 :room-6]}, :class-3 {:class-teacher :teacher-3, :class-room [:building-1 :room-2]}, :class-5 {:class-teacher :teacher-5, :class-room [:building-1 :room-3]}, :class-1 {:class-teacher :teacher-1, :class-room [:building-1 :room-1]}, :class-7 {:class-teacher :teacher-7, :class-room [:building-1 :room-4]}, :class-6 {:class-teacher :teacher-6, :class-room [:building-2 :room-3]}, :class-9 {:class-teacher :teacher-9, :class-room [:building-1 :room-5]}}})

;; Ergebnisse:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Untersuchung%20der%20Skalierbarkeit][Untersuchung\ der\ Skalierbarkeit:1]]

(defn create-scalable-optimizeable-room-scheduling-problem [num-hours num-days num-classes]
  (let [num-slots (* num-days num-hours)]
    {:classes (reduce #(assoc %1 (keyword (str "class-" %2))
                              {:class-teacher (keyword (str "teacher-" %2))
                               :class-room [(keyword (str "building-" %2)) :room-1]})
                      {} (range num-classes))
     :teachers (reduce #(assoc %1 (keyword (str "teacher-" %2)) {}) {} (range num-classes))
     :distances {}
     :num-days num-days
     :hours-per-day num-hours
     :buildings (reduce #(assoc %1 (keyword (str "building-" %2)) {:room-1 {}})
                        {} (range num-classes))
     :events (reduce #(assoc %1 (keyword (str "ef-" %2))
                             (create-event :classes (keyword (str "class-" %2))
                                           :teachers #{(keyword (str "teacher-" %2))}
                                           :number-of-times (dec num-slots)))
                     {} (range num-classes))}))

;; Untersuchung\ der\ Skalierbarkeit:1 ends here

;; [[file:~/programming/timetabling/timetabling.org::*Diskussion%20der%20Ergebnisse%20und%20Ausblick][Diskussion\ der\ Ergebnisse\ und\ Ausblick:1]]

(deftest test-solves
  (is (not (nil? (solve-problem example-entities))))
  (is (not (nil? (solve-problem bigger-example-entities)))))

;; Diskussion\ der\ Ergebnisse\ und\ Ausblick:1 ends here
