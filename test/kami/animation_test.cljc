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

(deftest editable-hermite-tangents
  (let [ka (a/keyframe 0 0 :hermite {:tangent-out 20})
        kb (a/keyframe 1 10 :linear {:tangent-in 0})
        track (a/track :x [ka kb])]
    (is (= 7.5 (a/sample track 0.5)))
    (let [automatic (a/auto-tangents (a/track :x [(a/keyframe 0 0) (a/keyframe 1 10) (a/keyframe 2 0)]))]
      (is (= 10 (:keyframe/tangent-out (first (:track/keyframes automatic)))))
      (is (= 0 (:keyframe/tangent-in (second (:track/keyframes automatic))))))))

(deftest playback-loop-and-rate
  (let [track (a/track :x [(a/keyframe 0 0) (a/keyframe 4 4)])
        looping (a/timeline 4 [track] {:loop-start 1 :loop-end 3 :loop? true :playback-rate 2})]
    (is (= 2 (a/playback-time looping 2)))
    (is (= {:x 2} (a/evaluate-playback looping 2)))
    (is (= 4 (a/playback-time (a/timeline 4 [track]) 10)))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (a/timeline 4 [track] {:loop-start 3 :loop-end 2})))))
