---
date: 2024-01-20 17:40:18.850144-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05D8\u05D9\u05E4\u05D5\u05DC\
  \ \u05D1\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E1\
  \u05DB\u05DF \u05D0\u05EA \u05D4\u05E4\u05E8\u05DE\u05E0\u05E0\u05D8\u05D9\u05D9\
  \u05DD. \u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05E2\u05D5\u05E9\u05D5\u05EA\
  \ \u05D6\u05D0\u05EA \u05DC\u05D7\u05D9\u05E9\u05D5\u05D1\u05D9\u05DD \u05D6\u05DE\
  \u05E0\u05D9\u05D9\u05DD, \u05D2\u05D9\u05D1\u05D5\u05D9 \u05D1\u05DE\u05E6\u05D1\
  \u05D9\u05DD \u05E9\u05DC \u05E7\u05E8\u05D9\u05E1\u05D4, \u05D5\u05D8\u05D9\u05E4\
  \u05D5\u05DC \u05D1\u05E4\u05E8\u05D8\u05D9\u05D5\u05EA \u05D4\u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD."
lastmod: '2024-02-25T18:49:37.048914-07:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\
  \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E1\u05DB\
  \u05DF \u05D0\u05EA \u05D4\u05E4\u05E8\u05DE\u05E0\u05E0\u05D8\u05D9\u05D9\u05DD\
  . \u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05E2\u05D5\u05E9\u05D5\u05EA \u05D6\
  \u05D0\u05EA \u05DC\u05D7\u05D9\u05E9\u05D5\u05D1\u05D9\u05DD \u05D6\u05DE\u05E0\
  \u05D9\u05D9\u05DD, \u05D2\u05D9\u05D1\u05D5\u05D9 \u05D1\u05DE\u05E6\u05D1\u05D9\
  \u05DD \u05E9\u05DC \u05E7\u05E8\u05D9\u05E1\u05D4, \u05D5\u05D8\u05D9\u05E4\u05D5\
  \u05DC \u05D1\u05E4\u05E8\u05D8\u05D9\u05D5\u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני מאפשרת טיפול בנתונים מבלי לסכן את הפרמננטיים. תוכניות עושות זאת לחישובים זמניים, גיבוי במצבים של קריסה, וטיפול בפרטיות הנתונים.

## איך לעשות:
איך יוצרים קובץ זמני ב-Clojure? הנה דוגמה:

```Clojure
(require '[clojure.java.io :as io])

(let [temp-file (io/file (io/temp-dir) "my-temp-file.txt")]
  (spit temp-file "התוכן שלי הזמני")
  (slurp temp-file))
```

פלט לדוגמה:
```
"התוכן שלי הזמני"
```

## עומק ים:
יצירת קובץ זמני נעשית מאז ימי הדיסקטים לצורך שמירת מצב ולהבטחת נקיון במערכות קבצים. ב-Clojure, משתמשים ב-java.io.File ליצירת קובץ זמני או אפילו על ידי יצירת סטרים זמני לנתונים הזרמתיים. יש גם ספריות חיצוניות המספקות אפשרויות נוספות ונוחות יותר.

## גם ראו:
- [Clojure java.io documentation](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [דוקומנטציה של חבילת java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [מדריך לספרייה nio.file של Java](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
