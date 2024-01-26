---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:35:40.157966-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח תאריך ממחרוזת הוא התהליך שבו אנו ממירים טקסט למבנה תאריך מובנה. תכנתים עושים זאת כדי לאפשר עיבוד וניתוח של נתוני תאריכים בצורה יעילה ונכונה.

## איך לעשות:
מוטב העסקה הוא תוספת `java-time` המתממשקת עם Java Time API בקלות.

```Clojure
(require '[java-time :as jt])

(defn parse-date [date-str]
  (jt/local-date date-str))

(parse-date "2023-04-01")
;; => #object[java.time.LocalDate 0x4e64842e "2023-04-01"]
```
כעת יש לנו עצם `LocalDate` שאיתו אפשר לעבוד בקלות.

## צלילה לעומק
פעם היינו משתמשים ב`java.util.Date` אך ה-API של Java Time החדש הוא הרבה יותר רובוסטי ונוח. התוספת `java-time` מנצלת זאת לטובת מתכנתי Clojure ומציעה ממשק נעים ופונקציונלי.

אלטרנטיבה לכך היא דרך `clj-time` שהיא ספרייה פופולרית גם כן, אך היא נחשבת כיום לפחות עדכנית לאחר הופעת `java-time`.

במידת הצורך, ניתן לעבוד ישירות עם Java API ללא תוספים, אך דורש זאת היכרות עמוקה יותר של ה-Java Interop.

## ראה גם
- [java-time GitHub](https://github.com/dm3/clojure.java-time)
- [Java Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
