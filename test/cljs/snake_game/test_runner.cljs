(ns snake-game.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [snake-game.core-test]
   [snake-game.common-test]))

(enable-console-print!)

(doo-tests 'snake-game.core-test
           'snake-game.common-test)
