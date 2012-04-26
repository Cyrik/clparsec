(ns clparsec.primitives
  (:use [clojure.algo.monads][clparsec.core])
  (:require [clojure [set :as set]]))

(defn >>= [p mf]
  (fn [state]
    (let [reply1 (p state)]
      (if (success? reply1)
        (let [p2 (mf (:result reply1))
              reply2 (p2 (:state reply1))]
          (merge-reply-errors reply1 reply2))
        reply1))))

(defn >>! [p x]
  (fn [state]
      (assoc (p state) :result x)))

(defn >>| [p q]
  (fn [state]
    (let [reply1 (p state)]
      (if (success? reply1)
        (let [reply2 (q (:state reply1))]
          (merge-reply-errors reply1 reply2))
      reply1))))

(defn |>> [p q]
  (fn [state]
    (let [reply1 (p state)]
      (if (success? reply1)
        (let [reply2 (q (:state reply1))]
          (assoc reply1 :errors (merge-errors (:errors reply1) (:errors reply2)),
                 :status (:status reply2)
                 :state (:state reply2)))
        reply1))))

(defn |>>| [p q]
  (fn [state]
    (let [reply1 (p state)]
      (if (success? reply1)
        (let [reply2 (q (:state reply1))
              errors1 (:errors reply1)
              errors2 (:errors reply2)]
          (assoc reply2 :errors (merge-errors (:errors reply1) (:errors reply2))
                 :result (list (:result reply1) (:result reply2))))
        reply1))))



(defn between [op cl p]
  (fn [state]
    (let [reply1 (op state)]
      (if (success? reply1)
        (let [reply2 (p (:state reply1))]
          (if (success? reply2)
            (let [reply3 (cl (:state reply2))]
              (assoc reply2 :errors (merge-errors reply1 reply2 reply3)
                     :status (:status reply3)
                     :state (:state reply3)))
            (merge-reply-errors reply1 reply2)))
        reply1))))

(defn ||>> [p f]
  (fn [state]
    (let [reply (p state)]
      (if (success? reply)
        (assoc reply :result (f (:result reply)))
        reply))))

(defn pipe [& xs]
  (let [f (first xs)
        parsers (rest xs)]
    (fn [state]
      (loop [parsers parsers
             s state
             acc nil]
        (if-let [parser (first parsers)]
          (let [reply (parser s)]
            (if (success? reply)
              (recur (rest parsers) (:state reply) (conj acc reply))
              (apply merge-reply-errors (reverse (conj acc reply)))))
          (assoc (peek acc) 
                 :result (apply f (map :result acc))
                 :errors (apply merge-errors (:errors acc))))))))

;;;;;;;;;;;;;;;; parsing alternatives ;;;;;;;;;;;;;;;;

(defn <|> [p q] 
  (fn [state]
    (let [reply1 (p state)]
      (if (or (success? reply1) (not=(:state reply1) state))
        reply1
        (merge-reply-errors reply1 (q state))))))

(defn choice [& parsers]
  (fn [state]
    (loop [parsers parsers
           replys nil]
      (if (empty? parsers)
        replys
        (let [p (first parsers)
              reply (p state)]
          (if (or (success? reply) (not= (:state reply) state))
            (merge-reply-errors replys reply)
            (recur (rest parsers) (merge-reply-errors replys reply))))))))

;(defn choiceL [& parsers]
;  (fn [state]))

(defn <|>! [parser x]
  (fn [state]
    (let [reply (parser state)]
      (println reply)
      (if (or (success? reply) (not= state (:state reply)))
        reply
        (make-success state x (:errors reply))))))

(defn opt [p]
  (fn [state]
    (let [reply (p state)]
      (if (success? reply)
        reply
        (if (=(:state reply) state)
          (assoc reply :result nil, :status :success)
          (assoc reply :result nil))))))

(defn optional [p]
  (fn [state]
    (let [reply (p state)]
      (if (or (success? reply) (not= state (:state reply)))
        (assoc reply :result nil :status :success)
        (assoc reply :result nil)))))

(defn attempt [parser]
  (fn [state]
    (let [reply (parser state)]
      (if (success? reply)
        reply
        (assoc reply :status :fail, :state state)))))


;;;;;;;;;;;;;;;;;;;;; conditional parsing and look ahead ;;;;;;;;

;;;;;;;;;;;;;;;;; customizing errors ;;;;;;;;;;;;;;;;


(defn <?> [parser label]
  (fn [state]
    (let [reply (parser state)]
      (if (= state (:state reply))
        (swap-error-messages (expected-list label))
        reply))))
    

;;;;;;;;;;;;;;; seq parsers ;;;;;;;;;;;;;;;;;;

(def tuple (partial pipe list))

(defn parray [n parser]
  (fn [state]
    (loop [i 0
           s state
           acc nil]
      (if (< i n)
          (let [reply (parser s)]
            ;(println reply)
            (if (success? reply)
              (recur (inc i) (:state reply) (conj acc reply))
              (apply merge-reply-errors (reverse (conj acc reply)))))
          (assoc (peek acc) 
                 :result (map :result acc)
                 :errors (apply merge-errors (:errors acc)))))))

;(defn skip-array)
(defn many-internal [result-from-empty parser]
  (fn [state]
    (let [frst-reply (parser state)]
      (if (success? frst-reply)
        (loop [s (:state frst-reply)
               acc (list frst-reply)]
          (let [reply (parser s)]
            ;(println reply)
            (if (success? reply)
              (recur (:state reply) (conj acc reply))
              (if (= (:state reply) s)
                (make-success s (concat (map :result acc)) (:errors (apply merge-reply-errors (reverse (conj acc reply)))))
                (assoc reply :errors (apply merge-reply-errors (reverse (conj acc reply))))))))
        (if (and (= (:state frst-reply) state) result-from-empty)
          (make-success state result-from-empty)
          frst-reply)))))
        
(def many (partial many-internal true))
(def many1 (partial many-internal false))

;(defn skip-many)
;(defn skip-many1)

(defn sep-by-internal [result-from-first fold-state result-from-rest result-from-empty sep-may-end parser-p parser-s]
  (fn [state]
    (let [frst-reply (parser-p state)]
      (if (success? frst-reply)
        (loop [s (:state frst-reply)
               acc-r (result-from-first (:result frst-reply))
               acc-e (:errors frst-reply)]
          (let [reply-s (parser-s s)
                reply-p (parser-p (:state reply-s))]
            ;(println reply-s "\n\n" reply-p "\n\n" acc-e)
            ;(println "!!!!!!!!    " acc-r)
            (if (and (success? reply-s) (success? reply-p))
              (recur (:state reply-p) 
                     (fold-state acc-r (:result reply-s) (:result reply-p))
                     (conj acc-e (:errors reply-s) (:errors reply-p)))
              (if (and (not (success? reply-s)) (= (:state reply-s) s)) ;didnt parse seperator as final one and sep didnt consume?
                (make-success s (result-from-rest acc-r) 
                              (apply merge-errors (reverse (conj acc-e (:errors reply-s))))) 
                (let [errors (apply merge-errors (reverse (conj acc-e (:errors reply-s)(:errors reply-p))))]
                  (if (and sep-may-end (not(success? reply-p)) (= (:state reply-s)(:state reply-p))) 
                    (make-success (:state reply-s) (result-from-rest acc-r) errors)
                    (if-not (success? reply-p)
                      (assoc reply-p :errors errors)
                      (assoc reply-s :errors errors))))))))
        (if (and (= (:state frst-reply) state) result-from-empty)
          (make-success state result-from-empty)
          frst-reply)))))

(defn- sep-by-fold  [a b c]
  (conj a c))

(defn- ignore [& xs] ())

(def sep-by (partial sep-by-internal list sep-by-fold reverse () false))
(def sep-by1 (partial sep-by-internal list sep-by-fold reverse false false))

(def skip-sep-by (partial sep-by-internal ignore ignore identity () false))
(def skip-sep-by1 (partial sep-by-internal ignore ignore identity false false))

(def sep-end-by (partial sep-by-internal list sep-by-fold reverse () true))
(def sep-end-by1 (partial sep-by-internal list sep-by-fold reverse false true))

(def skip-sep-end-by (partial sep-by-internal ignore ignore identity () true))
(def skip-sep-end-by (partial sep-by-internal ignore ignore identity false true))

(def chainl1 (partial sep-by-internal identity (fn [a b c] (b a c)) identity false false))
(defn chainl [p op x]
  (<|>! (chainl1 p op) x))

(defn create-parser-forwarded-to-atom []
  (let [res (atom (constantly nil))]
    (list #(@res %) res)))