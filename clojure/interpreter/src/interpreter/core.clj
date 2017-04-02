(ns interpreter.core
    (:gen-class)
    (:require [clojure.string :as string]
     :require [clojure.walk :as walk]))

; simply split up the line into tokens
(defn tokenize [line]
    (string/split line #"\s"))

(def add? #(= "+" %))
(def assign? #(= "=" %))

(defn num? [s]
    (every? #(Character/isDigit %) s))

(defn parse-num [s]
    (if (num? s)
        (try (java.lang.Long/parseLong s)
            (catch NumberFormatException nfe))))

(defn ident? [s]
    (every? #(Character/isLetter %) s))

(defn parse-ident [s]
    (if (ident? s) s))

(defn make-ast [operators operands]
    (if (empty? operators)
        (first operands)
    ; else
        (let [operator (first operators)
              right (first operands)
              left (first (rest operands))
              node (list operator left right)]
            (recur (rest operators) (conj (rest (rest operands)) node)))))

; Simpler shunting-yard algorithm to convert infix to an abstract syntax tree
; Does not support parentheses or associativity or order of operations
(defn shunt [tokens operators operands]
    (let [operator? #(or (add? %) (assign? %))
          token (first tokens)]

    (cond
        (empty? tokens)
            (make-ast operators operands)
        (operator? token)
            (recur (rest tokens) (conj operators token) operands)
        :else
            (recur (rest tokens) operators (conj operands token)))))
    
(defn parse [tokens]
    (shunt tokens () ()))

(defn eval-expr [table expr]
    (cond
        (list? expr)
            (let [op (first expr)
                  left (second expr)
                  right (last expr)]

            (if (add? op)
                (+ (eval-expr table left) (eval-expr table right))))
        (num? expr)
            (let [n (parse-num expr)]
                n)
        (ident? expr)
            (let [i (parse-ident expr)]
                (recur table (get table i)))))

(defn execute [asts]
    ; we assume op here is =
    (let [table (apply merge (map (fn [[op left right]] {left right}) asts))]
        (walk/walk (fn [[v expr]] [v (eval-expr table expr)]) identity table)))

(defn interpret [program]
    (let [tokens (map tokenize program)
          parsed (map parse tokens)]

    (execute parsed)))

(defn evaluate [program]
    (interpret (string/split-lines program)))

(defn run [file]
    (evaluate (slurp file)))

(defn printSolved [solved]
    (let [sorted (into (sorted-map) solved)]
        (doseq [[k v] sorted] (println (str k " = " v)))))

(defn -main
    [& args]
    (let [[file] args
         solved (run file)]
        (printSolved solved)))
