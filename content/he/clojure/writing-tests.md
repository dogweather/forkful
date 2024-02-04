---
title:                "כתיבת בדיקות"
date:                  2024-02-03T19:30:26.406495-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
