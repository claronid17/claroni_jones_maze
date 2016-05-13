(ns robert-jones.simulator)


(def maze1
  '(
     [| | | | | | | | | |]
     [| * _ _ _ _ _ _ F |]
     [| | | | | | | | | |]))

(def maze2
  '(
     [| | | | | | | | | |]
     [| _ * _ _ _ _ _ F |]
     [| | | | | | | | | |]))

(defn get-maze-symbol
  "Returns the item in the maze at the row, column location."
  [maze row column]
  (nth (nth maze row) column))

(defn get-maze-symbol
  [maze pos-vec]
  (nth (nth maze (first pos-vec)) (second pos-vec)))

(defn get-player
  "Returns coordinates of the player in the maze"
  [maze]
  (loop [layer 0]
    (if (= (some #{'*} (nth maze layer)) '*)
      [(.indexOf (nth maze layer) '*) layer] ;[column row]
      (recur (inc layer)))))

(defn check-for-wall
  "Checks if there is a wall in the direction of the attempted move."
  [maze move]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))]
    (cond
      (= move :U) (not (= '| (get-maze-symbol maze [(- player-column 1) player-row])))
      (= move :D) (not (= '| (get-maze-symbol maze [(+ player-column 1) player-row])))
      (= move :L) (not (= '| (get-maze-symbol maze [player-column (- player-row 1)])))
      (= move :R) (not (= '| (get-maze-symbol maze [player-column (+ player-row 1)])))
      )))

(defn switch-rows
  "moves character between rows."
  [source destination col]
  (loop [new-source []
         new-destination []
         index 0]
    (cond 
      (= index (count source)) [new-source new-destination]
      (= index col) (recur (conj new-source '_) (conj new-destination '*) (inc index))
      :else (recur (conj new-source (nth source index)) 
                   (conj new-destination (nth destination index)) 
                   (inc index)))))

(defn move-player
  [maze move]
  (cond
    (= move :U) (loop [new-maze []
                       row 0]
                  (if (= row (count maze))
                    new-maze
                    (if (= row (second (get-player maze)))
                      (loop [this-row []
                             column 0]
                        (if (= column (count (nth maze this-row)))
                          (conj new-maze this-row)
                          (recur (conj (nth (nth new-maze this-row) column)) 
                                       (inc column))))
                      (recur (conj new-maze (nth maze row)) (inc row)))))))

(defn move-player
  [maze move]
  (loop [new-maze []
         row 0]
    (if (= row (count maze))
      new-maze
      (if (= row (+ (second (get-player maze)) 1))
        (switch-rows (nth maze (second (get-player maze))) (nth maze
                    
  

;tester
(get-maze-symbol maze1 1 1)
(get-player maze1)
