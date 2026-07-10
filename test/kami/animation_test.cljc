(ns kami.animation-test (:require [clojure.test :refer [deftest is]] [kami.animation :as a]))
(deftest samples-and-clamps-keyframes
  (let [t (a/track :cube/x [(a/keyframe 0 0) (a/keyframe 2 10)])]
    (is (= 0 (a/sample t -1))) (is (= 5 (a/sample t 1))) (is (= 10 (a/sample t 3)))
    (is (= {:cube/x 5} (a/evaluate (a/timeline 2 [t]) 1)))))
