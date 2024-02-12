---
title:                "עבודה עם JSON"
aliases:
- /he/clojure/working-with-json.md
date:                  2024-02-03T19:23:07.651172-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON (JavaScript Object Notation) ב-Clojure כוללת ניתוח מחרוזות JSON למבני נתונים של Clojure (מפות, וקטורים) ולהיפך. משימה זו היא יסודית עבור שירותי רשת, API-ים, ויישומים שצריכים לתקשר נתונים בפורמט טקסטואלי מובנה, מכיוון ש-JSON מוכר ונתמך באופן כללי בסביבות תכנות שונות.

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
