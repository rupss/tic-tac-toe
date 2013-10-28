(ns tic-tac-toe.core
  (:gen-class))

; For printing the board
(def board-symbol-map
  { nil "_" :x "X" :o "O" })

; To keep track of whose turn it is
(def whose-turn
  { 0 :x 1 :o} )

(def letter-vec
  ["a" "b" "c"])

; The computer's player
(def computer :o)

(defn get-empty-board
  "Returns an empty board"
  []
  (into [] (map #(into [] %1) (partition 3 (repeat 9 nil)))))

(defn print-board
  "Prints board to screen"
  [board]
  (println "\n | A B C")
  (println "---------")
  (dotimes [n (count board)]
    (println (str n "|")
     (apply str
      (interpose " " (map (fn [x] (board-symbol-map x)) (nth board n)))))))

(defn parse-letter
  "letter is a string. Translates the letter to the appropriate number (a-0, b-1, c-2)
  so it be used to access the correct column in the board. Returns the 
  col number if letter is valid, otherwise nil"
  [letter]
  (let [num (.indexOf letter-vec letter)]
    (if (<= 0 num 2)
      num
      nil)))

(defn parse-int
  "number is a string - should be a string of an int. Returns the number (as an int)
  if number is valid, otherwise nil"
  [number]
  (try 
    (let [num (. Integer parseInt number)]
      (if (<= 0 num 2)
        num
        nil))
    (catch Exception e 
      nil)))

(defn get-square
  "Move will have the format <letter><number> e.g A1
  Refers to <column><row>"
  [move]
  (if
    (not (== (count move) 2))
    nil
    (do 
      (let [col (parse-letter (str (nth move 0)))
        row (parse-int (str (nth move 1)))
        ]
        (if (or (nil? row) (nil? col))
          nil
          (list row col))))))

(defn get-modified-row
  "row and col are the indices of the move to make. This function returns the 
  whole row in which the move was made, with the modified move"
  [board row col curr-player]
  (let [working-row (nth board row)]
    (if (not (nth working-row col))
      (into [] (assoc working-row col curr-player))
      working-row)))

(defn update-board
  "Returns the board updated with the new move"
  [board row col curr-player]
  (into [] (assoc board row (get-modified-row board row col curr-player))))

(defn check-horizontal-winner
  "Checks if there are 3 in a row of player in board"
  [board player]
  (some (fn[row] (every? (fn[col] (= col player)) row)) board))

(defn get-vertical-vectors
  "Returns a vector of 3 vectors. Each 'subvector' contains the items of a column of board,
  in order"
  [board]
  (loop [n 0
   result []]
   (if (> n 2)
    result
    (recur (+ n 1) (conj result (into [] (map (fn[x] (nth x n)) board)))))))

(defn check-vertical-winner
  "Returns true if there are 3 in a row of player vertically in board, false if not"
  [board player]
  (check-horizontal-winner (get-vertical-vectors board) player))

(defn get-diagonal-vectors
  "Returns a vector of 2 vectors. Each 'subvector' contains the items in a diagonal, in
  order"
  [board]
  (vector (vector (get-in board [0 0])
    (get-in board [1 1])
    (get-in board [2 2]))
  (vector (get-in board [0 2])
    (get-in board [1 1])
    (get-in board [2 0]))))

(defn check-diagonal-winner
  "Returns true if player has 3 in a row diagonally, false if not."
  [board player]
  (check-horizontal-winner (get-diagonal-vectors board) player))

(defn has-player-won
  "Returns true if player has won, false if not"
  [board player]
  (cond 
    (nil? player)
    nil
    :else
    (or (check-horizontal-winner board player)
      (check-vertical-winner board player)
      (check-diagonal-winner board player))))

(defn get-winner-message
  "Returns a string message of the winner"
  [winner]
  (str "Player " (board-symbol-map winner) " has won. Congrats!"))

(defn get-curr-player-message
  "Returns a string message of who's turn it is currently"
  [curr-player]
  (let [base (str "Current player: " (board-symbol-map curr-player))]
    (if (= curr-player :o)
      (str base " (the computer)")
      base)))

(defn is-board-full
  "Returns true if the board is full, false if not"
  [board]
  (not (some (fn[row] (some (fn[col] (nil? col)) row)) board)))

(defn get-prev-player
  "Returns the previous player"
  [n]
  (whose-turn (mod (- n 1) 2)))

(defn get-other-player
  [curr-player]
  (cond 
    (= curr-player :x) :o
    (= curr-player :o) :x))

(defn utility
  "Utility function for the minimax algorithm.
  :x is the max player and :o is the min player"
  [board]
  (cond
    (has-player-won board :x) 1
    (has-player-won board :o) -1
    (is-board-full board) 0
    :else nil))

(defn get-nil-indices
  [row]
  "For a particular row, returns the indices of its nil items."
  (keep-indexed #(if (nil? %2) %1) row))

(defn attach-row-coord
  "Given a row-coord (e.g 2) and an items list (e.g (0 1)), returns a list of lists where
  row-coord is attached to each item e.g ( (2 0) (2 1))"
  [row-coord items-list]
  (map #(list row-coord %) items-list))

(defn get-empty-squares
  [board]
  "Returns a list of (row, col) lists that are the coordinates of the nil squares in board"
  (apply concat (keep-indexed #(attach-row-coord %1 %2)
    (map get-nil-indices board))))

(defn get-all-actions
  [board curr-player]
  "Returns a list of all possible actions (boards) for the curr-player from board"
  (map (fn[x] (update-board board (first x) (last x) curr-player)) (get-empty-squares board)))

(defn get-comparison-fn
  [curr-player]
  "Returns the appropriate comparison function for minimax"
  (cond 
    (= curr-player :x) >
    (= curr-player :o) <))

(defn print-arr-boards
  [arr-boards]
  "For debugging purposes"
  (doseq [item arr-boards]
    (print-board item)))

(defn print-pairs
  "For debugging purposes"
  [pairs player board comp-fn]
  (println "PAIRS")
  (doseq [item pairs]
    (println "*****")
    (print-board (first item))
    (println (last item))
    (println "*****"))
  (println "BOARD")
  (print-board board)
  (println "PLAYER")
  (println player)
  (println "COMP FN")
  (println comp-fn))

(defn minimax
  [curr-board curr-player counter]
  "Recursive minimax implementation."
  (let [curr-util (utility curr-board)]
    (if (not (nil? curr-util))
      curr-util
      (do 
        (let [actions (get-all-actions curr-board curr-player)
          comp-fn (get-comparison-fn curr-player)]
          (let [action-util-pairs (map (fn[a] (list a (minimax-recursive a (get-other-player curr-player) (inc counter)))) actions)
                best-one (first (sort-by last comp-fn action-util-pairs))]
            (if (== counter 1)
              (first best-one)
              (last best-one))))))))

(defn apply-move
  [board curr-player]
  "If the move is valid, returns the updated board. Otherwise, returns the unmodified board.
   If it's the computer's turn, the computer plays based on the minimax algorithm."
  (if (= curr-player computer)
    (do
      (minimax board curr-player 1))
    (do
      (println "Your move: (<letter><number>)")
      (let [square (get-square (read-line))]
        (println)
        (cond
          (nil? square)
          board
          :else
          (update-board board (first square) (last square) curr-player))))))

(defn -main
  "Tic-tac-toe main method"
  [& args]
  (println "Starting tic-tac-toe game.")
  (loop [n 0
   board (get-empty-board)]
   (let [curr-player (whose-turn (mod n 2))
    prev-player (get-prev-player n)]
    (println (get-curr-player-message curr-player))
    (print-board board)
    (cond
        ; Base case #1 - if the previous player won
        (has-player-won board prev-player)
        (println (get-winner-message prev-player))
        ; Base case #2 - if the board is full (no more valid moves to make)
        (is-board-full board)
        (println "Draw - game over. Better luck next time!")
        :else 
        ; Recursive step
        (do 
          (let [new-board (apply-move board curr-player)]
            (cond 
              (= new-board board)
              (do 
                (println "Invalid move. Try again\n")
                ; was an invalid move, so keep the same player i.e n stays the same
                (recur n board)) 
              :else ; was a valid move, so move on to next player i.e increment n
              (recur (+ n 1) new-board))))))))



