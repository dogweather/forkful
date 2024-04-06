---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:37.425644-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05D5\u05E4\u05E8\u05D1\u05D9\u05DC\u05D9\u05D5\u05EA \u05D4\u05D7\
  \u05DC\u05E7\u05D4 \u05E9\u05DC Clojure \u05E2\u05DD Java \u05DE\u05D0\u05E4\u05E9\
  \u05E8\u05EA \u05DC\u05DA \u05DC\u05D2\u05E9\u05EA \u05D9\u05E9\u05D9\u05E8\u05D5\
  \u05EA \u05DC-API \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA-\u05E9\u05E2\u05D4\
  \ \u05E9\u05DC Java. \u05DB\u05DA \u05EA\u05D5\u05DB\u05DC \u05DC\u05E7\u05D1\u05DC\
  \ \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\
  \u05D9."
lastmod: '2024-03-13T22:44:38.721947-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\u05D5\u05E4\u05E8\u05D1\u05D9\u05DC\
  \u05D9\u05D5\u05EA \u05D4\u05D7\u05DC\u05E7\u05D4 \u05E9\u05DC Clojure \u05E2\u05DD\
  \ Java \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA \u05DC\u05D2\u05E9\u05EA\
  \ \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC-API \u05E9\u05DC \u05EA\u05D0\u05E8\
  \u05D9\u05DA-\u05E9\u05E2\u05D4 \u05E9\u05DC Java."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

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
