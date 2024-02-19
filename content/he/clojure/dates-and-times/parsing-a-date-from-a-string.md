---
aliases:
- /he/clojure/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:18.016754-07:00
description: "\u05DC\u05E4\u05E8\u05E1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Clojure \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\
  \u05DC\u05D4 \u05E9\u05DC \u05D4\u05DE\u05E8\u05EA \u05D4\u05E6\u05D2\u05D5\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D5\u05EA \u05E9\u05DC \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD \u05DC\
  \u05E6\u05D5\u05E8\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05E9\u05D9\u05DE\u05D5\u05E9\
  \u05D9\u05EA (\u05DC\u05DE\u05E9\u05DC, \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\
  \ DateTime \u05E9\u05DC Clojure). \u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\u05D4 \u05D4\
  \u05D5\u05D0\u2026"
lastmod: 2024-02-18 23:08:52.490700
model: gpt-4-0125-preview
summary: "\u05DC\u05E4\u05E8\u05E1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D1-Clojure \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\u05DC\
  \u05D4 \u05E9\u05DC \u05D4\u05DE\u05E8\u05EA \u05D4\u05E6\u05D2\u05D5\u05EA \u05D8\
  \u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D5\u05EA \u05E9\u05DC \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD \u05DC\u05E6\
  \u05D5\u05E8\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05E9\u05D9\u05DE\u05D5\u05E9\u05D9\
  \u05EA (\u05DC\u05DE\u05E9\u05DC, \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 DateTime\
  \ \u05E9\u05DC Clojure). \u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\u05D4 \u05D4\u05D5\
  \u05D0\u2026"
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
לפרס תאריך ממחרוזת ב-Clojure הוא פעולה של המרת הצגות טקסטואליות של תאריכים וזמנים לצורה יותר שימושית (למשל, אובייקט DateTime של Clojure). תהליך זה הוא יסודי לעיבוד נתונים, רישום (לוגינג), או כל יישום המניפולט נתונים זמניים, מאפשר לתכנתים לבצע משימות של פעולה, השוואה, או מניפולציה על תאריכים ביעילות.

## איך לעשות:
Clojure, בתור שפה המבוססת על JVM, מאפשרת לך להשתמש ישירות בספריות התאריכים והזמנים של Java. בואו נתחיל עם השילוב הטבעי של Java ואז נבדוק איך להשתמש בספרייה צד שלישי פופולרית, clj-time, לפתרונות יותר אידיומטיים ב-Clojure.

### שימוש בשילוב עם Java
Clojure יכולה להשתמש ישירות ב-`java.time.LocalDate` של Java לפרס תאריכים ממחרוזות:
```clojure
(require '[clojure.java.io :as io])

; פרס תאריך באמצעות שילוב עם Java
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; פלט: 2023-04-01
```

### שימוש ב-clj-time
ספרייה יותר אידיומטית לטיפול בתאריכים וזמנים ב-Clojure היא `clj-time`. היא עוטפת את Joda-Time, ספרייה מקיפה לפעולות עם תאריכים וזמנים. ראשית, תצטרכו להוסיף את `clj-time` לתלות של הפרויקט שלכם. הנה איך אתם פורסים מחרוזת תאריך באמצעות `clj-time`:

```clojure
; הקפידו להוסיף את [clj-time "0.15.2"] ל-project.clj שלכם תחת :dependencies

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; הגדרת פורמטר
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; פלט: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

דוגמאות אלה מדגימות פרס תאריך בסיסי. שני השיטות שימושיות, אך `clj-time` יכולה לספק גישה יותר ממוקדת ב-Clojure עם פונקציונליות נוספת לדרישות מורכבות יותר.
