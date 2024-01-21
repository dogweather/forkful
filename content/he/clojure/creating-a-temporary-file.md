---
title:                "יצירת קובץ זמני"
date:                  2024-01-20T17:40:18.850144-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/creating-a-temporary-file.md"
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