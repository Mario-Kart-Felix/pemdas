(ns pemdas.core
  (:require [clojure.string :as s :only [trim]])
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(def ^{:private true} exit-tokens #{"exit" "quit" "bye"})

(def ^{:private true} running (atom true))

(defn- should-exit-on [input]
  (if (or (nil? input) ;; handle Ctrl-D
          (exit-tokens (s/trim input)))
    (do (println "Bye!")
        (swap! running (fn [& args] false)))
    input))

(defn- display-instructions []
  (println "Calculator 1.0")
;;   (println calc/instructions)
  (println "Type" (apply str (interpose ", "  exit-tokens))
           "or Ctrl-D to exit."))

(def prio
  {'+ 0 ; Define operator priority here
   '- 0
   '* 1
   '/ 1
   'l -1
   'r -1
   'dummy -2})

(def operators #{'+ '- '* '/ 'sqr})

(defn pre-process [s]
  "Seperate operands with operators and replace ( with l, ) with r"
  (re-seq #"\d+|[\+\-\*\/lr]"
          (clojure.string/replace s #"\(|\)" {"(" "l" ")" "r"})))

(defn calc-once [stk]
  "Take one operator from operator stack and apply it to
  top two numbers in operand stack"
  (let [opt (:opt stk)
        num (:num stk)
        tmp-num (pop (pop num))
        tmp-opt (pop opt)
        last-two-num [(peek (pop num)) (peek num)]
        last-opt (peek opt)]
    (assoc stk
           :num (conj tmp-num (apply (eval last-opt) last-two-num))
           :opt tmp-opt)))

(defn process-stk [stk checker fn-ret]
  (loop [stk stk]
    (if (checker stk)
      (recur (calc-once stk))
      (fn-ret stk))))

(defn- sqr
  "Uses the numeric tower expt to square a number"
  [x]
  (math/expt x 2))

(defn calc
  "A simple calculator"
  [s]
  (try
      (process-stk
        (reduce
          (fn [stk item]
            (let [item (read-string item)
                  add-to-num #(assoc %1 :num (conj (:num %1) %2))
                  add-to-opt #(assoc %1 :opt (conj (:opt %1) %2))
                  item-prio (get prio item)
                  last-prio #(get prio (peek (:opt %)))]
              (cond
                (number? item) ; It's number
                (add-to-num stk item)
                (get operators item) ; It's operator
                (process-stk stk #(<= item-prio (last-prio %))
                             #(add-to-opt % item))
                (= 'l item) ; (
                (add-to-opt stk item)
                (= 'r item) ; )
                (process-stk stk #(not= (peek (:opt %)) 'l)
                             #(assoc % :opt (pop (:opt %))))
                :else
                (println "Unexpected syntax: " item))))
          (apply (partial list {:num '() :opt '(dummy)}) ;; Basic structure of stack
                 s))
        #(> (count (:opt %)) 1)
        #(peek (:num %)))
       (catch Exception e (str "Caught Exception: " (.getMessage e)))))
;; (calc pre-process (read-line))


(defn -main [& args]
  (display-instructions)
  (loop [input (read-line)]
    (when-not (or (nil? input)
                  (exit-tokens (s/trim input)))
      (println "Enter Expression:")
      (println "=> " (format "%.3f" (double (calc (pre-process (read-line)))))) ;; <!--print float -->
       (recur (read-line))))
  (println "Bye!"))
;;     (println "Enter an expression:")
;;     (if-let [input (should-exit-on (read-line))]
;;       (println "=> " (calc input)))))


;; (defn -main [& args]
;;   (display-instructions)
;;   (while @running
;;     (println "Do you want to do a calculation?")
;;     (should-exit-on (read-line))
;;     (println "enter expression:")
;;     (println "=> " (calc (pre-process (read-line))))
;;     ))
;;     (if-let [input (should-exit-on (read-line))]
;; ;;     (let [input (read-line)])
;;       (if (or (nil? input) ;; handle Ctrl-D
;;             (exit-tokens (s/trim input)))
;;         (do (println "Bye!")
;;           (swap! running (fn [& args] false)))
;; ;;       (println "enter expression:")
;;       (println "=> " (calc (pre-process (read-line))))))))
;;     (if-let [input (should-exit-on (read-line))]
;;       (println "=> " (calc (pre-process (read-line)))))))




;; (defn -main [& args]
;; ;;     (loop [input (get-input "What is your decision?")]
;; ;; ;;       (calc pre-process (str input))
;; ;;       (println input)
;;   (println "Pema Calculator")
;;   (while true
;;     (let[ input (read-line)]
;;     (println(calc (pre-process (input)))))))
