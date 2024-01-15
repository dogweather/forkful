---
title:                "השוואת שתי תאריכים"
html_title:           "Clojure: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

בניגוד לשפות תכנות אחרות, קלוז'ור (Clojure) מציעה כמה דרכים שונות להשוואת שתי תאריכים. השוואת תאריכים נחשבת לפעולה חשובה בתחום התכנות ויכולה לסייע בכתיבת קוד יעיל יותר ונקי יותר.

## איך לעשות זאת

הראה כיצד להשוות בין שני תאריכים בעזרת קלוז'ור באמצעות הדוגמה הבאה:

```Clojure
(def date1 (java.util.Date. 2020 10 1))
(def date2 (java.util.Date. 2020 11 1))
(if (< date1 date2)
  (println "date1 קטן מתאריך 2")
  (println "date1 גדול או שווה לתאריך 2"))
```

פלט של הדוגמה הזאת יהיה "date1 קטן מתאריך 2". הפעולות הסטנדרטיות המשמשות להשוואת תאריכים הן >, <, = ו-!=. בנוסף, ניתן להשתמש בפונקציות הקיימות של קלוז'ור כמו קטן מתאריך (before?) וגדול מתאריך (after?). לפני שתמשיך, ייתכן שתרצה להשתמש במערכת תאריכים אחרת כמו Joda Time או Java Time. במידה כזו, ניתן להמיר את התאריכים למרכיבי תאריך של Joda Time או לסטרינג המתאים.

## חפירה עמוקה

בכדי לבצע השוואה בין שני תאריכים בקלוז'ור, יש להשתמש בספריות של Java. קלוז'ור מאפשרת לכתוב קוד נקי וקריא יותר על ידי שימוש בשפת Java יחד עם Clojure. כמובן, אם תרצה, תוכל להשתמש בספריות נוספות כמו Joda Time או Java Time.

## ראה גם

- [מדריך לקלוז'ור למתחילים](https://clojure.org/guides/getting_started)
- [התיעוד הרשמי של קלוז'ור](https://clojure.org/index)
- [פורום קלוז'ור רשמי](https://ask.clojure.org/)
- [מדריך לשפת Clojure](https://practicalli.github.io/clojure/)