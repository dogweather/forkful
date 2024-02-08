---
title:                "פרסום תאריך ממחרוזת"
aliases:
- he/clojure/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:18.016754-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
