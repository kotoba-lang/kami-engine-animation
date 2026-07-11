(ns kami.animation-test (:require [clojure.test :refer [deftest is]] [kami.animation :as a]))
(deftest samples-and-clamps-keyframes
  (let [t (a/track :cube/x [(a/keyframe 0 0) (a/keyframe 2 10)])]
    (is (= 0 (a/sample t -1))) (is (= 5 (a/sample t 1))) (is (= 10 (a/sample t 3)))
    (is (= {:cube/x 5} (a/evaluate (a/timeline 2 [t]) 1)))))

(deftest editable-keyframes-and-interpolation
  (let [ka (a/keyframe 0 0 :step)
        kb (a/keyframe 2 10)
        tl (a/timeline 2 [(a/track :cube/x [ka kb])])]
    (is (= 0 (get (a/evaluate tl 1) :cube/x)))
    (let [moved (a/move-keyframe tl :cube/x (:keyframe/id kb) 4)]
      (is (= 4 (:keyframe/time (second (:track/keyframes (first (:timeline/tracks moved))))))))
    (is (empty? (:timeline/tracks
                 (-> tl
                     (a/delete-keyframe :cube/x (:keyframe/id ka))
                     (a/delete-keyframe :cube/x (:keyframe/id kb))))))))

(deftest smooth-interpolation
  (let [t (a/track :x [(a/keyframe 0 0 :smooth) (a/keyframe 1 10)])]
    (is (= 1.5625 (a/sample t 0.25)))
    (is (= 8.4375 (a/sample t 0.75)))))
