---
title:                "Clojure: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## למה

מדוע לעסוק בעבודה עם JSON? כשאנחנו מטפלים במאפיינים באתרים, אנחנו תמיד נתקלים בהרבה נתונים בפורמט JSON. עם זאת, עבודה עם JSON מאפשרת לנו להתאים במפתחות וערכים שונים, מה שמאפשר לנו להיפרד מטיפול ידני של הנתונים ולהיות יעילים יותר בפיתוח.

## איך לעשות זאת

התחלנו את הפרויקט שלנו עם מפתח כמו כלשהו, כן? ועכשיו, בעזרת שפת Clojure, אתה יכול ליצור מפתחים דינאמיים ולקשר אותם לטובת ניהול הנתונים שלך. נדגים זאת בעזרת קוד כמו הבא:

```Clojure
(ns json-examples
  (:require [clojure.data.json :as json]))

(def sample-json {"name": "Jane", "age": 25, "hobbies": ["reading", "painting", "hiking"]})

(println (:name sample-json)) ; outputs "Jane"

(println (json/write-str sample-json)) ; outputs {"name": "Jane", "age": 25, "hobbies": ["reading", "painting", "hiking"]}

```

מה אנחנו עושים פה? בשני הדוגמאות, אנחנו משתמשים בפעולות שונות כדי לגשת לנתונים בטבלת JSON. בשורה השנייה, אנחנו משתמשים בפונקציית :require כדי לייבא את הספריה של Clojure שמטפלת בנתוני JSON. אנחנו משתמשים בפונקציות כמו :write-str כדי לכתוב את הנתונים שלנו כמחרוזת בפורמט JSON.

## להעמיק

עם שפת Clojure, יש לנו גם אפשרויות מתיחת ידיים כדי לנהל מאפיינים שונים של נתוני JSON. לדוגמה, באמצעות הפונקציות המובנות של Clojure, ניתן לייצב נתונים לפי סדר אלפביתי, לעשות בדיקת תקינות ולהחיל פעולות חשבוניות על הנתונים.

אנחנו גם יכולים להשתמש בספריות צד שלישי כדי לייעל עוד יותר א