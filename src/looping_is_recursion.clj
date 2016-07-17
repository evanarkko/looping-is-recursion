(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
  				(if (zero? k)
  					acc
  					(recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
	(cond
		(== (count a-seq) 1)
			(first a-seq)
		(empty? a-seq)
			nil
		:else
			(last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond 
  	(not (== (count seq1) (count seq2))) false
  	(and (empty? seq1) (empty? seq2)) true
  	(== (first seq1) (first seq2))
  		(seq= (rest seq1) (rest seq2))
  	:else false))

(defn find-first-index [pred a-seq]
  (loop [ind 0 p pred b-seq a-seq]
  	(cond
  		(empty? b-seq) nil
  		(p (first b-seq)) ind
  		:else (recur (inc ind) p (rest b-seq)))))

(defn avg [a-seq]
  (loop [sum 0 amount 0 b-seq a-seq]
  	(if (empty? b-seq)
  		(/ sum amount)
  		(recur (+ sum (first b-seq)) (inc amount) (rest b-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  	(disj a-set elem)
  	(conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{} b-seq a-seq]
  	(if (empty? b-seq)
  		a-set
  		(recur (toggle a-set (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
	(if (zero? n)
		n
		(loop [prev 0 curr 1 ind 1]
			(if (== ind n)
				curr
				(recur curr (+ prev curr) (inc ind))))))

(defn cut-at-repetition [a-seq]
  (loop [a-vec [] a-set #{} b-seq a-seq]
  	(if (or (empty? b-seq) (contains? a-set (first b-seq)))
  		a-vec
  		(recur (conj a-vec (first b-seq)) (conj a-set (first b-seq)) (rest b-seq)))))

