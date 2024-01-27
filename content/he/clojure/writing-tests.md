---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות (tests) היא תהליך שבו אנו בודקים את התנהגות הקוד שלנו. מתכנתים עושים זאת כדי לוודא שהקוד עובד כראוי ולזהות באגים לפני פרסום התוכנה.

## איך לעשות:
```Clojure
; הוספת ספריית הבדיקות
(require '[clojure.test :refer :all])

; דוגמה לבדיקה פשוטה
(deftest test-addition
  (testing "בדיקת חיבור"
    (is (= 5 (+ 2 3)))))

; הרצת הבדיקות
(run-tests)

; פלט דוגמה
; Testing user
; 
; Ran 1 tests containing 1 assertions.
; 0 failures, 0 errors.
```

## עומק השקיעה
ההיסטוריה של בדיקות תוכנה מתחילה בשנות ה-50. אלטרנטיבות לספריית `clojure.test` כוללות את `Midje` ו`Speclj`. בחינת האימפלמנטציה היא חשובה כדי להבין לעומק איך הבדיקות משפיעות על התהליך המלא של פיתוח התוכנה.

## גם ראה
- [clojure.test documentation](https://clojure.github.io/clojure/clojure.test-api.html)
- [Midje on GitHub](https://github.com/marick/Midje)
- [Speclj on GitHub](https://github.com/slagyr/speclj)
