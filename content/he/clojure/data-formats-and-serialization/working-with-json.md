---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:07.651172-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Clojure \u05D0\u05D9\
  \u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D5\u05EA \u05DC\u05E2\u05D1\u05D5\u05D3\
  \u05D4 \u05E2\u05DD JSON, \u05D5\u05DC\u05DB\u05DF \u05D1\u05D3\u05E8\u05DA \u05DB\
  \u05DC\u05DC \u05D9\u05E9 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\
  \u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9. `cheshire`\
  \ \u05D5-`jsonista` \u05D4\u05DF \u05D1\u05D7\u05D9\u05E8\u05D5\u05EA \u05E4\u05D5\
  \u05E4\u05D5\u05DC\u05E8\u05D9\u05D5\u05EA \u05D1\u05E9\u05DC\u2026"
lastmod: '2024-03-13T22:44:38.739345-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D5\u05EA \u05DC\
  \u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON, \u05D5\u05DC\u05DB\u05DF \u05D1\
  \u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05D9\u05E9 \u05DC\u05D4\u05E9\u05EA\u05DE\
  \u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\
  \u05E9\u05D9."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## איך לעשות:
Clojure אינה כוללת פונקציות מובנות לעבודה עם JSON, ולכן בדרך כלל יש להשתמש בספריות צד שלישי. `cheshire` ו-`jsonista` הן בחירות פופולריות בשל קלות השימוש שלהן והביצועים.

### שימוש ב-Cheshire
ראשית, הוסף את Cheshire לתלות הפרויקט שלך ב-`project.clj`:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

לנתח מחרוזת JSON למפת Clojure ולהמיר מפה למחרוזת JSON:

```clj
(require '[cheshire.core :as json])

;; ניתוח מחרוזת JSON למפת Clojure
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; המרת מפת Clojure למחרוזת JSON
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### שימוש ב-Jsonista
הוסף את Jsonista לפרויקט שלך `project.clj`:
```clj
[jsonista "0.3.2"]
```

פעולות דומות עם Jsonista:

```clj
(require '[jsonista.core :as j])

;; ניתוח מחרוזת JSON ל-Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; המרת מפת Clojure למחרוזת JSON
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

בשתי הספריות, יש לך את האופציה לקודד ולפענח מבני נתונים מורכבים יותר, וישנן פונקציות ופרמטרים נוספים שמאפשרים התאמה אישית של תהליכי הסיריאליזציה והדה-סיריאליזציה. עבור רוב היישומים, הפונקציונליות שהוצגה מספקת בסיס חזק לעבודה עם JSON ביישומי Clojure.
