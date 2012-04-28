(ns clparsec.core
  (:use clojure.algo.monads)
  (:require [clojure [set :as set]]))


(def ^:dynamic *newline-chars* #{\return \newline "\r\n"})
(def ^:dynamic *whitespace* #{\space \tab \newline \return \formfeed})
(defprotocol AReply
  (success? [r]))

(defrecord Reply
  [status result errors state]
  AReply
  (success? [this] (if (= status :success) true false)))

(defrecord ErrorMessage
  [type message])

(defrecord ParseError
  [position messages])

(defprotocol ALocation
  (location-code [location]))

(extend-protocol ALocation
  Integer (location-code [position] (format "position %s" position)))

(defrecord LineLocation [line]
  ALocation (location-code [this] (format "line %s" line)))

(defprotocol ALineAndColumnLocation
  (inc-line [location])
  (inc-column [location])
  (plus-column [location n])
  (plus-line [location n]))

(defrecord StandardLocation [line column]
  ALocation
    (location-code [this] (format "line %s, column %s" line column))
  ALineAndColumnLocation
    (inc-line [this] (assoc this :line (inc line), :column 1))
    (inc-column [this] (assoc this :column (inc column)))
    (plus-column [this n] (assoc this :column (+ column n)))
    (plus-line [this n] (assoc this :line (+ line n))))

(defn make-standard-location [line column]
  {:pre #{(integer? line) (integer? column)}}
  (StandardLocation. line column))

(defn standard-alter-location [character]
  (if (*newline-chars* character)
    inc-line inc-column))

(defprotocol AState
  "The protocol of FnParse states, which must
  be able to return a position."
  (get-remainder [state])
  (position [state])
  (skip-one [state])
  (skip [state c])
  (next-state [state])
  (location [state])
  (peep [state])
  (skip-whitespace [state])
  (skip-strn [state strn])
  (read-chars-or-newlines-while [state pred1 pred normalize-n])
  (end? [state])
  (skip-newline [state])
  (read-char-or-newline [state])
  (user-state [state]))

(defrecord State
  [remainder position location user-state]
  AState
    (position [this] position)
    (get-remainder [this] remainder)
    (user-state [this] user-state)
    (location [this] location)
    (end? [this] (empty? remainder))
    (next-state [this]
      (when-let [remainder (seq remainder)]
        (assoc this
               :remainder (next remainder), :position (inc position),
               :location ((standard-alter-location (first remainder)) location))))
    (skip [this c] (when (=(first remainder)c)
                         (next-state this)))
    (skip-newline [this] 
                  (if (= \return (first remainder))
                    (if (= \newline (second remainder))
                      (assoc this 
                             :remainder (next (next remainder)), :position (+ position 2)
                             :location ((standard-alter-location "\r\n") location))
                      (next-state this))
                    (if (*newline-chars* (first remainder))
                      (next-state this))))
    (read-char-or-newline [this]
                          (if-let [c (first remainder)]
                            (if (= \return c)
                              (if (= \newline (second remainder))
                                (list (assoc this 
                                             :remainder (next (next remainder)), :position (+ position 2)
                                             :location ((standard-alter-location "\r\n") location))
                                      \newline)
                                (list (next-state this) \newline))
                              (list (next-state this) c))))
    (peep [this] (first remainder))
    (skip-one [this] (next-state this))
    (skip-whitespace [this] 
                     (when (*whitespace* (first remainder))
                       (loop [state this]
	                       (let [c (peep state)]
	                         (if (= \return c)
	                           (if (= \newline (second (:remainder state)))
	                             (recur (assoc state 
	                                          :remainder (next (next (:remainder state))), :position (+ (:position state) 2)
	                                          :location ((standard-alter-location "\r\n") (:location state))))                                   
	                             (recur (next-state state)))
	                           (if (*whitespace* c)
	                             (recur (next-state state))
	                             state))))))
    (skip-strn [this strn] 
                 (let [length (count strn)]
                   (when (<= length (count remainder))
                     (loop [i 0]
                       (if (< i length)
                         (when (=(nth strn i) (nth remainder i))
                           (recur (inc i)))
                         (assoc this :remainder (.substring remainder i)
                                :position (+ position i)
                                :location (plus-column location i)))))))
    (read-chars-or-newlines-while [this pred1 pred normalize-n] ;todo normalize newlines
                                  (if-let [c (first remainder)]
                                    (if (pred1 c)
                                      (loop[state (next-state this)
                                            acc [(first remainder)]]
                                        (if-let [frst (peep state)]                                              
                                          (if (pred frst)
                                            (recur (next-state state) (conj acc frst))
                                            (list (apply str acc) state))
                                          (list (apply str acc) state)))
                                      (list "" this))
                                    (list "" this))))


(defn make-state
  "Creates a state with the given parameters."
  [input & {:keys #{location user-state}
            :or {location (make-standard-location 1 1)}}]
  (State. input 0 location user-state))


(defn- merge2-errors
  "Returns the union of the error messages if the replys are at the same position
   and the errors of reply2 otherwise."  
  ([error1 error2]
    (if (and error1 (= (:position error1) (:position error2)))
      (assoc error1 :messages
             (set/union (:messages error1) (:messages error2)))
      (if-not error2 error1 error2))))
(defn merge-errors
  "Returns the union of the error messages if the replys are at the same position
   and the errors of reply2 otherwise."  
  ([& errors]
    (when errors
      (reduce merge2-errors (reverse errors)))))

(defn union-error [& errors]
  (assoc (first errors) :messages (set/union (map :messages errors))))

(defn merge-reply-errors
  ([reply] reply)
  ([reply1 reply2]
    ;(println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"reply1 "\n" reply2 "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    (if (<(:position (:state reply1))(:position (:state reply2)))
      reply2
      (if (>(:position (:state reply1))(:position (:state reply1)))
        reply1
        (assoc reply2 :errors (merge2-errors (:errors reply1) (:errors reply2))))))
  ([reply1 reply2 & replys]
    ;(println reply1 reply2 replys)
    (let [replys (conj replys reply2)]
      ;(println reply1 replys)
      (loop [reply reply1
             replys replys]
        (if-let [reply2 (first replys)]
          (recur (merge-reply-errors reply reply2) (rest replys))
          reply)))))

(defn make-failure 
  ([state]
    (Reply. :fail nil nil state))
  ([state errors]
    (Reply. :fail nil errors state)))

(defn make-fatal-error
  ([state]
    (Reply. :fatal nil nil state))
  ([state errors]
    (Reply. :fatal nil errors state)))

(defn make-success
  ([state]
    (Reply. :success nil nil state))
  ([state result]
    (Reply. :success result nil state))
  ([state result errors]
    (Reply. :success result errors state)))

(defn make-parse-error [state message]
  (ParseError. (position state) #{message}))


(defn expected [label]
  (ErrorMessage. :expected label))

(defn expected-list [label] (#{expected}))

(defn swap-error-messages [reply messages]
  (assoc reply :errors (assoc (:errors reply) :messages messages)))  

(defn run [p str]
  (p (make-state str)))