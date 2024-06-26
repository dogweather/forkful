---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:26.406495-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E4\u05E2\u05D5\u05DC: Clojure, \u05E9\u05DE\
  \u05E0\u05E6\u05DC\u05EA \u05D0\u05EA \u05D4-JVM, \u05EA\u05D5\u05DE\u05DB\u05EA\
  \ \u05D1\u05DE\u05D2\u05D5\u05D5\u05DF \u05E4\u05E8\u05D9\u05D9\u05DE\u05D5\u05D5\
  \u05E8\u05E7\u05D9\u05DD \u05DC\u05D1\u05D3\u05D9\u05E7\u05D5\u05EA. \u05E2\u05DD\
  \ \u05D6\u05D0\u05EA, \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05DE\u05D5\u05D1\u05E0\
  \u05D9\u05EA \u05E9\u05E0\u05DE\u05E6\u05D0\u05EA \u05D1\u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05E0\u05E4\u05D5\u05E5 \u05D4\u05D9\u05D0 `clojure.test`. \u05D4\u05E0\u05D4\
  \ \u05D3\u05D5\u05D2\u05DE\u05D0 \u05E4\u05E9\u05D5\u05D8\u05D4."
lastmod: '2024-03-13T22:44:38.709971-06:00'
model: gpt-4-0125-preview
summary: "Clojure, \u05E9\u05DE\u05E0\u05E6\u05DC\u05EA \u05D0\u05EA \u05D4-JVM, \u05EA\
  \u05D5\u05DE\u05DB\u05EA \u05D1\u05DE\u05D2\u05D5\u05D5\u05DF \u05E4\u05E8\u05D9\
  \u05D9\u05DE\u05D5\u05D5\u05E8\u05E7\u05D9\u05DD \u05DC\u05D1\u05D3\u05D9\u05E7\u05D5\
  \u05EA."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## איך לפעול:
Clojure, שמנצלת את ה-JVM, תומכת במגוון פריימוורקים לבדיקות. עם זאת, ספרייה מובנית שנמצאת בשימוש נפוץ היא `clojure.test`. הנה דוגמא פשוטה:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "תכונת החיבור"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
לאחר הרצת בדיקה זו, תראה פלט הדומה ל:

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

למי שמחפש אפשרויות עם יותר תכונות, ניתן לנצל ספריות של צד שלישי כמו `Midje` או `test.check`. הנה איך ייתכן להשתמש ב-Midje לבדיקה דומה:

ראשית, הוסף את Midje לתלות ב-project.clj שלך:
```clojure
[midje "1.9.9"]
```

אז, הבדיקה שלך עם Midje עשויה להראות כך:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "בדיקת חיבור"
  (add 2 2) => 4
  (add 3 4) => 7)
```

על ידי הרצת הבדיקה דרך Midje עם `lein midje`, הפלט יציג משהו בסגנון:

```
כל הבדיקות (2) הצליחו.
```
