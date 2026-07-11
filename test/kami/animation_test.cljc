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

(deftest hierarchical-skeleton-pose-evaluation
  (let [rig (a/skeleton [(a/bone :root "Root" nil {:translation [1 0 0]})
                         (a/bone :arm "Arm" :root {:translation [0 2 0]})
                         (a/bone :hand "Hand" :arm {:translation [0 1 0]})])
        matrices (a/bone-world-matrices rig (a/pose {:arm {:translation [0 3 0]}}))]
    (is (= 3 (count matrices)))
    (is (= [1.0 4.0 0.0] (mapv #(nth (get matrices :hand) %) [12 13 14])))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (a/bone-world-matrices rig (a/pose {:missing {:translation [0 0 0]}}))))))

(deftest skeleton-integrity
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (a/skeleton [(a/bone :a "A") (a/bone :a "Again")])))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (a/skeleton [(a/bone :a "A" :missing)])))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (a/skeleton [(a/bone :a "A" :b) (a/bone :b "B" :a)]))))

(deftest bone-tracks-evaluate-to-world-matrices
  (let [rig (a/skeleton [(a/bone :root "Root") (a/bone :hand "Hand" :root {:translation [0 1 0]})])
        target (a/bone-track-target :hand :translation :x)
        tl (a/timeline 2 [(a/track target [(a/keyframe 0 0) (a/keyframe 2 4)])
                          (a/track :object/x [(a/keyframe 0 9) (a/keyframe 2 9)])])
        pose (a/evaluate-skeleton-pose rig tl 1)
        matrices (a/evaluate-skeleton rig tl 1)]
    (is (= [2 0 0] (get-in pose [:pose/bones :hand :translation])))
    (is (= [2.0 0.0 0.0] (mapv #(nth (get matrices :hand) %) [12 13 14])))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (a/evaluate-skeleton rig
                                      (a/timeline 1 [(a/track [:bone :missing :rotation :z]
                                                              [(a/keyframe 0 0)])]) 0)))))

(deftest canonical-bone-target-validation
  (is (= [:bone :arm :rotation :z] (a/bone-track-target :arm :rotation :z)))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (a/bone-track-target :arm :opacity :x)))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (a/bone-track-target :arm :rotation :w))))

(deftest inverse-bind-skinning-matrices
  (let [rig (a/skeleton [(a/bone :root "Root" nil {:translation [2 0 0]})
                         (a/bone :child "Child" :root {:translation [0 3 0]})])
        identity [1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0]
        rest (a/bone-skinning-matrices rig (a/pose {}))
        moved (a/bone-skinning-matrices rig (a/pose {:root {:translation [4 0 0]}}))]
    (is (= [identity identity] rest))
    (is (= [2.0 0.0 0.0] (mapv #(nth (first moved) %) [12 13 14])))
    (is (= [2.0 0.0 0.0] (mapv #(nth (second moved) %) [12 13 14])))))
