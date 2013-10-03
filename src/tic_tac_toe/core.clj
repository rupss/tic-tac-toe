(ns tic-tac-toe.core
  (:gen-class))

(def board-symbol-map
	{ :empty "__" :x "X" :o "O" })

(defn get-empty-board
	[]
	  (into [] (repeat 3
	  	(into [] (repeat 3 :empty))))
	)

(defn print-board
	[board]
	(dotimes [n (count board)]
		(println 
			(apply str 
				(interpose " " (map (fn [x] (board-symbol-map x)) (nth board n))))
			)
	))

(defn -main
  "Tic-tac-toe main method"
  [& args]
  (println "Hello, World!")
  (print-board (get-empty-board)))



