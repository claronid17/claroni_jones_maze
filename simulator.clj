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

(defn get-player
  "Returns coordinates of the player in the maze"
  [maze]
  (loop [layer 0]
    (if (= (some #{'*} (nth maze layer)) '*)
      [(.indexOf (nth maze layer) '*) layer]
      (recur (inc layer)))))

(defn check-for-wall
  "Checks if there is a wall in the direction of the attempted move."
  [maze move]
    (let [player-pos (get-player maze)]
      (if

;tester
(get-maze-symbol maze1 1 1)
(get-player maze1)