(ns particlebox.core-test
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest)])
  (:require [cemerick.cljs.test :as t])
  (:use [particlebox.test-util :only (roughly=)]
        [particlebox.core :only (magnitude direction)]))

(deftest magnitude-test
  (is (roughly= (magnitude [1 2 3]) 3.742))
  (is (roughly= (magnitude [4 5 6]) 8.775))
  (is (roughly= (magnitude [7 8 9]) 13.928))
  (is (roughly= (magnitude [0 0 0]) 0))
  (is (roughly= (magnitude [0.577 0.577 0.577]) 1))
  (is (roughly= (magnitude [1.73 1.73 1.73]) 3)))

(deftest direction-test
  (is (roughly= (direction [1 2 3]) [0.267 0.534 0.802]))
  (is (roughly= (direction [1 2 3] 3) [0.802 1.603 2.405]))
  (is (roughly= (direction [4 5 6]) [0.456 0.569 0.684]))
  (is (roughly= (direction [4 5 6] 7) [3.191 3.989 4.786]))
  (is (roughly= (direction [7 8 9]) [0.502 0.574 0.646]))
  (is (roughly= (direction [7 8 9] 11) [5.528 6.318 7.108]))
  (is (roughly= (direction [0 0 0]) [0 0 0]))
  (is (roughly= (direction [0 0 0] 13) [0 0 0]))
  (is (roughly= (direction [0.577 0.577 0.577]) [0.577 0.577 0.577]))
  (is (roughly= (direction [0.577 0.577 0.577] 3) [1.73 1.73 1.73])))
