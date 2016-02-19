; Assignment 2 Grades

; 1 divisors 		10/10
; 2 Abundant Number 	10/10
; 3 abundent 300	10/10
; 4 pattern count	10/10
; 5 most-freguent-word	10/10
; 6 find-clumps		10/10
; 7 weather		9/10

; Total	69/70

;; QUESTION 1

(defn divisors [n]

  (filter #(=(mod n %) 0) (range 1 (+ n 1)))
  )


;; QUESTION 2

(defn abundance [n]

  (- (reduce + (divisors n)) (* 2 n))
  )


;; QUESTION 3

(defn abundant-numbers [n]
  (filter #(> (abundance %) 0) (range  n))
  )
(abundant-numbers 300)


;; QUESTION 4

(defn pattern-count [text pattern]

  (def pattern_length (count pattern))

  (if (> pattern_length 0)
    (get
      (frequencies
        (for [index (range 0  (-(+ 1(.length text)) pattern_length))]
          (subs text index (+ index pattern_length))
          )
        )
      pattern 0)

    0
    )
  )

;; QUESTION 5
(defn most-frequent-word [text n]

  (def my-result (
                   frequencies
                   (for [index (range 0  (-(+ 1(.length text)) n))]
                     (subs text index (+ index n))
                     )
                   )
    )

  (def max-value (apply max ( map val my-result)))

  (keys (filter #(= (second %) max-value) my-result))

  )

;; QUESTION 6

(defn t-frequent-word [text k t]

  (def my-result (
                   frequencies
                   (for [index (range 0  (-(+ 1(.length text)) k))]
                     (subs text index (+ index k))
                     )
                   )
    )

  (keys (filter #(= (second %) t) my-result))

  )

(defn find-clumps [text k L t]

  (
    distinct
    (
      apply concat
            (for [index (range 0  (-(+ 1(.length text)) L))]
              (t-frequent-word (subs text index (+ index L)) k t)
              )
            )
    )
  )


;; QUESTION 7

(declare convert-str-int)                                   ;added so code would compile

(defn process-lineitem [line]

  (def items (.split line "\t"))

  (if  (not= (clojure.string/blank? line) true)
    (if (> (count items) 2)
      (if (not= (nth items 0) "Dy")
        (into {} { :day (convert-str-int (nth items 0)) :spread (-(convert-str-int (nth items 1)) (convert-str-int (nth items 2)))})
        )
      )
    )
  )
; you need to declare this before you use it -1
;; standard name would be str->int
(defn convert-str-int [input]
  (Integer/parseInt (clojure.string/replace input #"\*" "") )
  )

(defn compare-lines [this that]
  (if (> (get this :spread -1) (get that :spread -1)) (into {} this) (into {} that))
  )

(defn  maximum-spread [path]

  (def lines (.split (slurp path) "\n"))

  (loop [lines lines acc {}]
    (if (= (seq lines) nil)
      (get acc :day nil)

      (recur (rest lines) (compare-lines acc (process-lineitem (first lines))))
      )
    )
  )



