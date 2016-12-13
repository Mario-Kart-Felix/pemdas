(ns pemdas.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
 )

(defn tokenize [expr]
  (let [to-chars #(clojure.string/split (clojure.string/replace % " " "") #"")
        is-digit? #(and % (re-find #"^\d+$" %))]
    (reverse
      (reduce
        (fn [[t & ts :as tokens] token]
          (if (and (is-digit? token) (is-digit? t))
            (cons (str t token) ts)
            (cons token tokens)))
        '(), (to-chars expr)))))

(defn shunting-yard [tokens]
  (let [ops {"+" 1, "-" 1, "*" 2, "/" 2, "^" 2}]
    (flatten
      (reduce
        (fn [[rpn stack] token]
          (let [less-op? #(and (contains? ops %) (<= (ops token) (ops %)))
                not-open-paren? #(not= "(" %)]
            (cond
              (= token "(") [rpn (cons token stack)]
              (= token ")") [(vec (concat rpn (take-while not-open-paren? stack))) (rest (drop-while not-open-paren? stack))]
              (contains? ops token) [(vec (concat rpn (take-while less-op? stack))) (cons token (drop-while less-op? stack))]
              :else [(conj rpn token) stack])))
        [[] ()]
        tokens))))

(defn pow [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))


(defn rpn [tokens]
  (let [ops {"+" +, "-" -, "*" *, "/" /, "^" pow}]
    (first
      (reduce
        (fn [stack token]
          (if (contains? ops token)
            (cons ((ops token) (second stack) (first stack)) (drop 2 stack))
            (cons (read-string token) stack)))
        [] tokens))))

(def log #(do (println %) %))

(def calc (comp rpn shunting-yard tokenize))

(def calc-debug (comp rpn log shunting-yard log tokenize))

(println (calc-debug "1 + 1  * 2"))
