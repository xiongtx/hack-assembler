(ns hack-assembler.core
  (:require
   [clojure.java.io :as io]
   [me.raynes.fs :as fs]
   [superstring.core :as str])
  (:import
   (java.io File)
   (org.apache.commons.lang3 StringUtils)))

(def comp-codes
  "Translation table for `comp`"
  {:0   "0101010"
   :1   "0111111"
   :-1  "0111010"
   :D   "0001100"
   :A   "0110000"
   :!D  "0001101"
   :!A  "0110001"
   :D+1 "0011111"
   :A+1 "0110111"
   :D-1 "0001110"
   :A-1 "0110010"
   :D+A "0000010"
   :D-A "0010011"
   :A-D "0000111"
   :D&A "0000000"
   :D|A "0010101"
   :M   "1110000"
   :!M  "1110001"
   :M+1 "1110111"
   :M-1 "1110010"
   :D+M "1000010"
   :D-M "1010011"
   :M-D "1000111"
   :D&M "1000000"
   :D|M "1010101"})

(def dest-codes
  "Translation table for `dest`"
  {nil  "000"
   :M   "001"
   :D   "010"
   :MD  "011"
   :A   "100"
   :AM  "101"
   :AD  "110"
   :AMD "111"})

(def jump-codes
  "Translation table for `jump`"
  {nil  "000"
   :JGT "001"
   :JEQ "010"
   :JGE "011"
   :JLT "100"
   :JNE "101"
   :JLE "110"
   :JMP "111"})

(def predefined-symbol-addresses
  "Memory addresses for predefined symbols."
  {:SP 0
   :LCL 1
   :ARG 2
   :THIS 3
   :THAT 4
   :R0 0
   :R1 1
   :R2 2
   :R3 3
   :R4 4
   :R5 5
   :R6 6
   :R7 7
   :R8 8
   :R9 9
   :R10 10
   :R11 11
   :R12 12
   :R13 13
   :R14 14
   :R15 15
   :SCREEN 16384
   :KBD 24576})

(def *symbol-table* (atom predefined-symbol-addresses))

(def *symbol-counter* (atom 16))

(defn- reset-table!
  "Reset symbol table and counter"
  []
  (reset! *symbol-table* predefined-symbol-addresses)
  (reset! *symbol-counter* 16))

(defn a-instruction?
  "Return true if line is an A-instruction."
  [^String line]
  (let [line (str/trim line)]
    (str/starts-with? line "@")))

(defn c-instruction?
  "Return true if line is a C-instruction"
  [^String line]
  (let [line (str/trim line)]
    (or (str/includes? line "=")
        (str/includes? line ";"))))

(defn label?
  "Return true if line is a label."
  [^String line]
  (let [line (str/trim line)]
    (and (str/starts-with? line "(")
         (str/ends-with? line ")"))))

(defn comment?
  "Return true if line is a comment."
  [^String line]
  (let [line (str/trim line)]
    (str/starts-with? line "//")))

(defn ignored?
  "Return true if line should be ignored."
  [^String line]
  (or (str/blank? line)
      (comment? line)))

(defn a-instruction->code
  [^String i]
  {:pre [(a-instruction? i)]
   :post [(or (= (count %) 16)
              (throw (ex-info "Length is not 16: " {:instruction i :code %})))]}
  (let [a (subs i 1)
        code (if (StringUtils/isNumeric a)
               (Integer/parseInt a)
               (let [k (keyword (str/upper-case a))]
                 (if (contains? @*symbol-table* k)
                   (k @*symbol-table*)
                   (do (swap! *symbol-table* assoc k @*symbol-counter*)
                       (swap! *symbol-counter* inc)
                       (k @*symbol-table*)))))]
    (str/pad-left (Integer/toBinaryString code) 16 "0")))

(defn c-instruction->code
  [^String i]
  {:pre [(c-instruction? i)]
   :post [(or (= (count %) 16)
              (throw (ex-info "Length is not 16: " {:instruction i :code %})))]}
  (let [i (if (not (str/contains? i "="))
            (str "=" i)
            i)
        [dest compute jmp] (->> (str/split i #"[=;]")
                                (map (comp #(when-not (str/blank? %) (keyword %))
                                           str/trim
                                           str/upper-case)))]
    (str "111"
         (get comp-codes compute)
         (get dest-codes dest)
         (get jump-codes jmp))))

(defn instruction->code
  [^String i]
  (cond
    (a-instruction? i) (a-instruction->code i)
    (c-instruction? i) (c-instruction->code i)))

(defn first-pass
  "First pass through file to get labels.

Returns symbol table with label entries added."
  [file symbol-table]
  (with-open [rdr (io/reader file)]
    (loop [lines (->> (line-seq rdr)
                      (remove ignored?)
                      (map (comp str/trim
                                 #(first (str/split % #"//")))))
           cnt 0
           table symbol-table]
      (let [line (first lines)]
        (if (nil? line)
          table
          (recur (next lines)
                 (if (label? line)
                   cnt
                   (inc cnt))
                 (if (label? line)
                   (let [k (-> line
                               (str/chop-prefix "(")
                               (str/chop-suffix ")")
                               (str/upper-case)
                               (keyword))]
                     (if-not (contains? table k)
                       (assoc table k cnt)
                       table))
                   table)))))))

(defn parse-file
  [file]
  ;; Reset symbol table and counter
  (reset-table!)

  ;; First pass
  (swap! *symbol-table* conj (first-pass file {}))

  ;; Second pass
  (with-open [rdr (io/reader file)]
    (loop [lines (->> (line-seq rdr)
                      (remove ignored?)
                      (remove label?)
                      (map (comp str/trim
                                 #(first (str/split % #"//")))))
           codes []]
      (let [line (first lines)]
        (if (nil? line)
          codes
          (recur (next lines) (conj codes (instruction->code line))))))))

(defn -main
  "Assemble `.asm` files into `.hack` files.

  The `.hack` files are placed in the same directory as the `.asm` files."
  [& args]
  (doseq [file args]
    (let [outfile (str (fs/parent file) File/separator (fs/name file) ".hack")]
      (with-open [w (io/writer outfile)]
        (doseq [line (parse-file file)]
          (.write w line)
          (.write w "\n"))))))
