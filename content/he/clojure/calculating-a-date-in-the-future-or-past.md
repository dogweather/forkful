---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Clojure: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא מעין פעולה שבה אנחנו מוסיפים או מחסירים ימים, שבועות, חודשים או שנים מדאת ספציפית. מתכנתים עושים זאת כשהם צריכים לעקוב אחרי מרקמים זמניים - לדוגמא, זמן שליחת דינאמי, ערך של תאריך פקיעה וכדומה.

## איך ל:
Clojure מספקת תמיכה מולדת במניפולציות תאריכים באמצעות ספריית java.time.

```clojure
(ns my.namespace
  (:import [java.time LocalDate]))

(defn add-days [date num-days]
  (.plusDays date num-days))

(def today (LocalDate/now))
(def ten-days-later (add-days today 10))

(prn "Today is " today)
(prn "In 10 days it will be " ten-days-later)
```
זה יגדיר שני תאריכים - היום ועוד 10 ימים - ואז ידפיס שני הם.

## בהרחבה:
- היסטוריה: נתמך בספריות הקודם של קלוז'ר.
- אלטרנטיבות: Clojure מאפשרת שימוש בספריות חיצוניות כמו clj-time או clojure.java-time.
- פרטי ביצוע: המניפולציה של תאריכים באמצעות plusDays מבוססת על מודל של unscaled addition, כלומר, היא תמיד מוסיפה מספר מסוים של ימים, ללא התחשבות בשינויים כמו מעבר לשנה הלועזית.

## ראו גם:
- תיעוד ספריית [java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) 
- פרויקט [clj-time](https://github.com/clj-time/clj-time) ב-GitHub 
- פרויקט [clojure.java-time](https://github.com/dm3/clojure.java-time) ב-GitHub