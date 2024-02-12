---
title:                "קבלת התאריך הנוכחי"
aliases:
- /he/clojure/getting-the-current-date/
date:                  2024-02-03T19:09:37.425644-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
השגת התאריך הנוכחי בתכנות היא קריטית מסיבות רבות, כולל רישום לוגים, חותמת זמן לאירועים, ותזמון משימות. ב-Clojure, ניב ליספ על JVM, משימה זו מנצלת את יכולות ה-interop עם Java, מה שמאפשר גישה ישירה ופשוטה ל-API העשיר של תאריך-שעה של Java.

## איך לעשות:

### באמצעות Java Interop
האינטרופרביליות החלקה של Clojure עם Java מאפשרת לך לגשת ישירות ל-API של תאריך-שעה של Java. כך תוכל לקבל את התאריך הנוכחי:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; פלט לדוגמה
(get-current-date) ; "2023-04-15"
```

### באמצעות ספריית clj-time
לפתרון יותר אידיומטי ב-Clojure, ייתכן ותבחר בספריית `clj-time`, מעטפת סביב Joda-Time, אך לרוב הפרויקטים החדשים מומלץ ה-API של תאריך-שעה של Java 8. עם זאת, אם תעדיף או תזדקק ל`clj-time`:

ראשית, הוסף את `clj-time` לתלות הפרויקט שלך. ב`project.clj` שלך, כלול:

```clojure
[clj-time "0.15.2"]
```

לאחר מכן, השתמש בה כדי לקבל את התאריך הנוכחי:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; פלט לדוגמה
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

שתי השיטות מספקות דרכים מהירות ויעילות לקבל את התאריך הנוכחי ב-Clojure, באמצעות ליחצן את כוחה של הפלטפורמה הבסיסית של Java או את הנוחות של ספרייה ספציפית ל-Clojure.
