(ns pegthing.core
  (require [clojure.set :as set])
  (:gen-class))

;;Create the board

(defn tri*
  "Generates a lazy seq of triangular numbers."
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n )]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the given number triangular"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of the row n"
  [n]
  (last (take n tri)))
  
(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn in-bounds?
  "Is every position less than or equal the max position?"
  [max-pos & positions]
  (every? (fn [pos] (>= max-pos pos)) positions))

(defn connect
      [board max-pos pos neighbor destination]
      (if (in-bounds? max-pos neighbor destination)
        (reduce (fn [new-board [p1 p2]] (assoc-in new-board [p1 :connections p2] neighbor))
                board
                [[pos destination] [destination pos]])
        board))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connector] (connector new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

;; Moving

(defn pegged?
  [board pos]
  (get-in board [pos :pegged]))

(defn valid-moves
  "Returns a map of all valid moves for pos, where the key is the 
  destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Return jumped postition if the move from p1 to p2 is valid, otherwise nil"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn remove-peg
  [board p]
  (assoc-in board [p :pegged] false))

(defn add-peg
  [board p]
  (assoc-in board [p :pegged] true))

(defn move-peg
 "Take peg out of pp1 and place it to p2"
 [board p1 p2]
 (add-peg (remove-peg board p1) p2))

