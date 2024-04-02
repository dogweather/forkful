---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:26.406495-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1-Clojure, \u05DB\u05DE\u05D5 \u05D1\u05E9\u05E4\u05D5\u05EA \u05EA\u05DB\
  \u05E0\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA, \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D3 \u05DE\u05D5\u05E7\u05D3\u05E9\
  \ \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05E9\u05D4\u05E7\u05D5\u05D3 \u05D4\u05E8\
  \u05D0\u05E9\u05D9 \u05E9\u05DC\u05DA \u05E4\u05D5\u05E2\u05DC \u05DB\u05E6\u05E4\
  \u05D5\u05D9. \u05D6\u05D4 \u05E2\u05D5\u05D6\u05E8 \u05D1\u05D4\u05D1\u05D8\u05D7\
  \u05EA \u05E0\u05DB\u05D5\u05E0\u05D5\u05EA, \u05DE\u05E7\u05DC \u05E2\u05DC \u05E9\
  \u05D9\u05E4\u05D5\u05E5 \u05E7\u05D5\u05D3 \u05D5\u05DE\u05E9\u05E4\u05E8\u2026"
lastmod: '2024-03-13T22:44:38.709971-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  -Clojure, \u05DB\u05DE\u05D5 \u05D1\u05E9\u05E4\u05D5\u05EA \u05EA\u05DB\u05E0\u05D5\
  \u05EA \u05D0\u05D7\u05E8\u05D5\u05EA, \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\
  \u05D9\u05E8\u05EA \u05E7\u05D5\u05D3 \u05DE\u05D5\u05E7\u05D3\u05E9 \u05DC\u05D0\
  \u05D9\u05DE\u05D5\u05EA \u05E9\u05D4\u05E7\u05D5\u05D3 \u05D4\u05E8\u05D0\u05E9\
  \u05D9 \u05E9\u05DC\u05DA \u05E4\u05D5\u05E2\u05DC \u05DB\u05E6\u05E4\u05D5\u05D9\
  . \u05D6\u05D4 \u05E2\u05D5\u05D6\u05E8 \u05D1\u05D4\u05D1\u05D8\u05D7\u05EA \u05E0\
  \u05DB\u05D5\u05E0\u05D5\u05EA, \u05DE\u05E7\u05DC \u05E2\u05DC \u05E9\u05D9\u05E4\
  \u05D5\u05E5 \u05E7\u05D5\u05D3 \u05D5\u05DE\u05E9\u05E4\u05E8\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## מה ולמה?
כתיבת בדיקות ב-Clojure, כמו בשפות תכנות אחרות, כוללת יצירת קוד מוקדש לאימות שהקוד הראשי שלך פועל כצפוי. זה עוזר בהבטחת נכונות, מקל על שיפוץ קוד ומשפר יציבות קוד.

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
