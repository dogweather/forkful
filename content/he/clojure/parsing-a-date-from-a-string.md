---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה זה אומר ולמה?
פארסינג תאריך ממחרוזת הוא התהליך שבו ממירים מחרוזת שמכילה תאריך לפורמט תאריך שהמחשב יכול לעבוד איתו. תכניתאים עושים זאת כדי להפוך את הנתונים לפורמט שיכול להשתלב בלוגיקה של התוכנה, מאפשר ניתוחים על התארים ומאפשר תצוגה אחידה של תאריך למשתמש הסופי.

## איך לעשות:
ניתן להשתמש במודול `clj-time` ב-Clojure לפארסינג של תאריכים.

```clojure
(require '[clj-time.format :as f])

(defn parse-date [date-str] 
  (f/parse (f/formatter "yyyy-MM-dd") date-str))

(parse-date "2022-01-30")
```

במקרה הזה, נקבל כתוצאה – `#object[org.joda.time.DateTime 2022-01-30T00:00:00.000Z]`.

## חקירה יותר מעמיקה:
1. היסטוריה: באופן כללי, פארסינג של תאריכים היה תמיד קשה עד שהתפתחו ספריות מסוימות שמקלות על התהליך.
2. אלטרנטיבות: יצרני Clojure יכולים לבחור בין מספר ספריות עיבוד תאריכים: `java.util.Date`, `java.time` ו-`clj-time`.
3. פרטי אמיתה: `clj-time` הוא ספרייה Clojure על פני Joda-Time (ספריית Java), המקלה על עיבוד תאריכים וזמנים ב-Clojure.

## קישורים נוספים:
1. [תיעוד clj-time](https://github.com/clj-time/clj-time)
2. [מדריך ל- java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
3. [מדריך ל- java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)