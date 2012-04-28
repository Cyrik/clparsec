(ns clparsec.test.helpers
  (:use [clojure.test])
  (:use [clparsec.core]))

(defn result-success-internal [with-newline content nskipped-chars result errors parser]
  (let [state (make-state content)
        res (parser state)]
    (do 
      (is (success? res))
      (is (= (:result res) result))
      (is (= (:messages (:errors res)) errors))
      (is (= (position (:state res)) nskipped-chars))
      (if with-newline
        (is (=(:line (location (:state res))) 2))))))

(defn rok 
  "result ok"
  [parser content n-skipped-chars result]
  (result-success-internal false content n-skipped-chars result nil parser))

(defn roke 
  "result ok with error messages"
  [parser content n-skipped-chars result errors]
  (result-success-internal false content n-skipped-chars result errors parser))

(defn roknl
  "result ok with newline"
  [parser content n-skipped-chars result ]
  (result-success-internal true content n-skipped-chars result nil parser))

(defn result-failure-internal [parser status content n-skipped-chars errors]
  (let [state (make-state content)
        res (parser state)]
    (do
      (is (= (:status res) status))
      (is (= (:messages (:errors res)) errors))
      (is (= (position (:state res)) n-skipped-chars)))))

(defn rfail [parser content n-skipped-chars errors]
  (result-failure-internal parser :fail content n-skipped-chars errors))

(defn check-parsers-equal-for [p1 p2 state]
  (let [res1 (p1 state)
        res2 (p2 state)]
    (when (not= res1 res2)
      (is (= (:status res1)(:status res2)))
      (is (= (:errors res1)(:errors res2)))
      (is (= (position(:state res1))(position(:errors res2))))
      (is (= (location(:state res1))(location(:errors res2))))
      (if (success? res1)
        (is (= (:result res1) (:result res2)))))))

(defn check-parsers-equal-for-str [p1 p2 str]
  (check-parsers-equal-for p1 p2 (make-state str)))

(defn constant-test-parsers [result errors]
  [#(make-success % result errors)
   #(make-success (assoc % :user-state (+ 1 (user-state %))) result errors)
   #(make-failure % errors)
   #(make-failure (assoc % :user-state (+ 1 (user-state %))) errors)])
    