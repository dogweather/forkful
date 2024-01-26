---
title:                "עבודה עם JSON"
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON הוא פורמט נתונים נפוץ להעברת מידע בין שרת ללקוח. תכנתי עושים שימוש בו מכיוון שהוא קריא, פשוט לניתוח ותואם למבנים ברוב שפות התכנות.

## איך לעשות:
כדי לעבוד עם JSON ב-Clojure, נשתמש בספרייה `cheshire`. התחילו עם התקנתה:

```Clojure
(require '[cheshire.core :as json])
```

עכשיו נפנק נתונים ל-JSON:

```Clojure
(json/generate-string {"name" "דוד" "age" 30})
; => "{\"name\":\"דוד\",\"age\":30}"
```

וננתח מ-JSON:

```Clojure
(json/parse-string "{\"name\":\"רחל\",\"age\":25}")
; => {"name" "רחל", "age" 25}
```

## היכרות עמוקה:
JSON נוצר באופן כמעט מקרי על ידי דאגלס קרוקפורד. יש כיום אלטרנטיבה נפוצה שנקראת XML, אך היא נחשבת יותר מסורבלת. ב-Clojure, ניתוח ויצירה של JSON מתבצעים באמצעות ספריות חיצוניות כגון `cheshire`, אשר משתמשת בג'אווה תחת המכסה לביצועים מהירים.

## ראו גם:
- [מדריך חשיר לתיעוד של שפת Clojure](https://clojuredocs.org/)
- [פרויקט Cheshire ב-GitHub](https://github.com/dakrone/cheshire)
