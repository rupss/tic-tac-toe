(ns tic-tac-toe.core
  (:gen-class))

(def board-symbol-map
	{ nil "__" :x "X" :o "O" })

(def whose-turn
	{ 0 :x 1 :o} )

(defn get-move
	[board row col]
	(nth (nth board row) col))

(defn nth-replace
	[original place replacement]
	(cond (empty? original) nil
		(= place 0)
		(cons replacement (rest original))
		true (cons (first original)
			(nth-replace (rest original) (- place 1) replacement))))

(defn get-empty-board
	[]
	  (into [] (repeat 3
	  	(into [] (repeat 3 nil)))))

(defn print-board
	"Prints board to screen. Right now, is very minimalistic and not super pretty."
	[board]
	(dotimes [n (count board)]
		(println 
			(apply str 
				(interpose " " (map (fn [x] (board-symbol-map x)) (nth board n)))))))

(defn num-is-valid
	"Returns true if 0 <= num < 3, false if not."
	[num]
	(if (and (>= num 0) (< num 3))
			true
			false))

(defn parse-letter
	"letter is a string"
	[letter]
	(let [num (- (int (nth (.toLowerCase letter) 0)) (int \a))]
		(if (num-is-valid num)
			num
			nil)))

(defn parse-int
	"number is a string - should be a string of an int"
	[number]
	(try 
		(let [num (. Integer parseInt number)]
			(if (num-is-valid num)
				num
				nil))
		(catch Exception e 
			nil)))

(defn get-square
	"Move will have the format <letter><number> e.g A1
	Refers to <row><column>"
	[move]
	(cond 
		(not (== (count move) 2))
			nil
		:else
			(do 
				(let [row (parse-letter (str (nth move 0)))
					  col (parse-int (str (nth move 1)))
					  ]
					(if (or (nil? row) (nil? col))
						nil
						(list row col))))))

(defn get-modified-row
	[board row col curr-player]
	(let [working-row (nth board row)]
		(if (not (nth working-row col))
			(into [] (nth-replace working-row col curr-player))
			working-row)))

(defn update-board
	[board row col curr-player]
	(into [] (nth-replace board row (get-modified-row board row col curr-player))))

(defn apply-move
	[board curr-player]
	(let [square (get-square (read-line))]
		(cond 
			(nil? square)
				board
			:else
				(update-board board (first square) (last square) curr-player))))

(defn check-horizontal-winner
	[board player]
	(some (fn[row] (every? (fn[col] (= col player)) row)) board))

(defn get-vertical-vectors
	[board]
	(loop [n 0
		   result []]
		(if (> n 2)
			result
			(recur (+ n 1) (conj result (into [] (map (fn[x] (nth x n)) board)))))))

(defn check-vertical-winner
	[board player]
	(check-horizontal-winner (get-vertical-vectors board) player))

(defn get-diagonal-vectors
	[board]
	(vector (vector (get-move board 0 0)
					(get-move board 1 1)
					(get-move board 2 2))
			(vector (get-move board 0 2)
					(get-move board 1 1)
					(get-move board 2 0))))

(defn check-diagonal-winner
	[board player]
	"Returns true if player has 3 in a row diagonally, false if not."
	(check-horizontal-winner (get-diagonal-vectors board) player))

(defn has-player-won
	[board player]
	"Returns true if player has won, false if not"
	(cond 
		(nil? player)
			nil
		:else
			(or (check-horizontal-winner board player)
			(check-vertical-winner board player)
			(check-diagonal-winner board player))))

(defn get-winner-message
	[winner]
	"Returns a string message of the winner"
	(str "Player " (board-symbol-map winner) " has won. Congrats!"))

(defn get-curr-player-message
	[curr-player]
	"Returns a string message of who's turn it is currently"
	(str "Current player: " (board-symbol-map curr-player)))

(defn is-board-full
	[board]
	"Returns true if the board is full, false if not"
	(not (some (fn[row] (some (fn[col] (nil? col)) row)) board)))

(defn get-prev-player
	[n]
	(whose-turn (mod (- n 1) 2)))

(defn -main
  "Tic-tac-toe main method"
  [& args]
  (println "Starting tic-tac-toe game.")
	(loop [n 0
		   board (get-empty-board)]
		(let [curr-player (whose-turn (mod n 2))
			  prev-player (get-prev-player n)]
			(println (get-curr-player-message curr-player))
			(println (print-board board))
			(cond 
				(has-player-won board prev-player)
					(println (get-winner-message prev-player))
				(is-board-full board)
					(println "Draw - game over. Better luck next time!")
				:else 
					(do 
						(let [new-board (apply-move board curr-player)]
							(cond 
								(= new-board board)
									(do 
										(println "Invalid move. Try again")
										(recur n board))
								:else 
									(recur (+ n 1) new-board))))))))



