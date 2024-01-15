---
title:                "כתיבת מבחנים"
html_title:           "Clojure: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## למה 

כתיבת בדיקות בתכנות היא תהליך חשוב וקריטי שעוזר לוודא שהקוד שלנו עובד כפי שצריך ויעיל מבחינה תקינתית. כתיבת בדיקות גם מפחיתה את הסיכונים של באגים ואזרחים בקוד ונותנת ביטחון לכתיבת קוד חדש.

## כיצד לעשות את זה

היכנסו לעולם הבדיקות בקלות עם Clojure! תחילה, נצור פרויקט חדש ונתקין את הספריות הדרושות. לאחר מכן, נתחיל בכתיבת קוד בפונקציה `clojure.test/deftest` כדי ליצור בדיקות חדשות. לשם נוחות, ניתן לעבוד בתוך "```Clojure ... ```" קוד בלוק כדי להראות את הקוד ופלט הצפוי לכל בדיקה. לדוגמה:

```Clojure
(require '[clojure.test :refer [deftest is]])

(deftest test-addition
  (is (= (+ 2 2) 4)))

;; => lein test test-addition
;; => Ran 1 tests containing 1 assertions.
;; => 0 failures, 0 errors.
```

ניתן להשתמש בפונקציות נוספות כגון `is-not` עבור בדיקות של אי-שוויון, `thrown?` עבור בדיקת קריאת חריגה, ועוד.

## לצאת למעמקים

כאשר אנחנו יוצרים בדיקות, חשוב להיות כמה שיותר דייקניים כדי לוודא שהקוד עובד כפי שצריך. ניתן להשתמש בפונקציות כמו `is-instance` כדי לבדוק את סוג הערך שמוחזר מהפונקציה, `is-satisfied?` כדי לבדוק את תנאי מסוים, ועוד.

## לראות גם

למידע נוסף על כתיבת בדיקות בקוד קלוז'ר, נשמח לראות אותך באתרים הבאים:

- [ClojureDocs - כתיבת בדיקות](https://clojuredocs.org/clojure.test/deftest)
- [Clojure for the Brave and True - בדיקות](https://www.braveclojure.com/testing/)
- [Learn Clojure - Test-Driven Development](https://www.learn-clojure.com/testing/)