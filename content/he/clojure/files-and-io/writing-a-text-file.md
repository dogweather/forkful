---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:12.010143-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D4 `spit` \u05D4\u05D9\u05D0 \u05D4\u05D3\u05E8\u05DA\
  \ \u05D4\u05E4\u05E9\u05D5\u05D8\u05D4 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05DC\u05DB\
  \u05EA\u05D5\u05D1 \u05D8\u05E7\u05E1\u05D8 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D1\
  -Clojure. \u05D4\u05D9\u05D0 \u05DC\u05D5\u05E7\u05D7\u05EA \u05E9\u05E0\u05D9 \u05D0\
  \u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\u05DD: \u05E0\u05EA\u05D9\u05D1 \u05D4\
  \u05E7\u05D5\u05D1\u05E5 \u05D5\u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05DB\u05EA\u05D9\u05D1\u05D4. \u05D0\u05DD \u05D4\u05E7\u05D5\u05D1\u05E5 \u05DC\
  \u05D0 \u05E7\u05D9\u05D9\u05DD, `spit`\u2026"
lastmod: '2024-03-13T22:44:38.734730-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `spit` \u05D4\u05D9\u05D0\
  \ \u05D4\u05D3\u05E8\u05DA \u05D4\u05E4\u05E9\u05D5\u05D8\u05D4 \u05D1\u05D9\u05D5\
  \u05EA\u05E8 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D8\u05E7\u05E1\u05D8 \u05DC\u05E7\
  \u05D5\u05D1\u05E5 \u05D1-Clojure."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## איך לעשות:


### כתיבת טקסט לקובץ באמצעות הפונקציות המובנות של Clojure
הפונקציה `spit` היא הדרך הפשוטה ביותר לכתוב טקסט לקובץ ב-Clojure. היא לוקחת שני ארגומנטים: נתיב הקובץ והמחרוזת לכתיבה. אם הקובץ לא קיים, `spit` תייצר אותו. אם הוא כן קיים, `spit` תדרוס אותו.

```clojure
(spit "example.txt" "שלום, עולם!")
```

להוספת טקסט לקובץ קיים, ניתן להשתמש בפונקציה `spit` עם האפשרות `:append`.

```clojure
(spit "example.txt" "\nבואו נוסיף שורה חדשה זו." :append true)
```

לאחר הרצת קטעי קוד אלו, "example.txt" יכיל:

```
שלום, עולם!
בואו נוסיף שורה חדשה זו.
```

### שימוש בספריות צד שלישי
למרות שהיכולות המובנות של Clojure לעיתים קרובות מספיקות, הקהילה פיתחה ספריות חזקות עבור משימות יותר מורכבות או ספציפיות. לצורך קלט/פלט של קבצים, אחת הספריות הפופולריות היא `clojure.java.io`, שמספקת גישה יותר דומה ל-Java לניהול קבצים.

על מנת להשתמש ב-`clojure.java.io` לכתיבה לקובץ, תחילה עליך לייבא אותה:

```clojure
(require '[clojure.java.io :as io])
```

לאחר מכן, תוכל להשתמש בפונקציה `writer` כדי לקבל אובייקט writer, ובפונקציה `spit` (או אחרות כמו `print`, `println`) לכתוב לקובץ:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "זה נכתב באמצעות clojure.java.io"))
```

זה ייצור (או ידרוס אם כבר קיים) "example_with_io.txt" עם הטקסט:

```
זה נכתב באמצעות clojure.java.io
```

זכור: `with-open` מבטיח שהקובץ נסגר כראוי לאחר הכתיבה, ומונע דליפות משאבים פוטנציאליות.
