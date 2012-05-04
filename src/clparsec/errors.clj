(ns clparsec.errors
  (:use [clparsec.core])
  (:require [clojure [set :as set]]))

(defrecord ErrorMessage
  [type message])

(defrecord ParseError
  [position location messages]
  Object
  (toString [this] (let [msg (reduce str (map :message messages))]
                     (str "expected: " msg " at location: " (location-code location)))))

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


(defn make-parse-error [state message]
  (ParseError. (position state) (location state) #{message}))

(defn expected [label]
  (ErrorMessage. :expected label))

(defn message-error [label]
  (ErrorMessage. :message label))

(defn expected-list [label] (#{expected}))

(defn swap-error-messages [reply messages]
  (assoc reply :errors (assoc (:errors reply) :messages messages)))  