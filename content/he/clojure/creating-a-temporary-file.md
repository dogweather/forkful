---
title:    "Clojure: יצירת קובץ זמני"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה:

יצירת קובץ זמני היא פעולה חשובה לתכנותנים בכל שפת תכנות. באמצעות הפעולה הזו, אנו יכולים ליצור קובץ שימושי בזמן הרצת הקוד, אשר ניתן להשתמש בו כדי לאחסן נתונים או לבצע פעולות מסוימות.

## כיצד לעשות זאת:

למטרות הדוגמה, נשתמש בפונקציה "with-open" המאפשרת לנו ליצור ולנהל קובץ זמני בקלות:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "my-temp-file-" ".txt")]
  (println "נוצר קובץ זמני בנתיב: " (.getAbsolutePath temp-file)))
```

פלט:

נוצר קובץ זמני בנתיב: /var/folders/mx/3qkmwzq11s51x1bz6nwrz4g80000gn/T/my-temp-file-3300242438988184509.txt

## עומק:

כאשר אנו יוצרים קובץ זמני ב- Clojure, הוא נמחק אוטומטית מכונת המחשב בסופו של דבר. גם אם התכנית שלנו נכשלת, יתרה הקובץ הולך לטיפול בסיום של התכנית, וזה אומר שאנו יכולים להשתמש בקובץ זמני את כל זמן הרצת הקוד הנתון.

כמו כן, ניתן להתאים אישית את הקבצים הזמניים שנוצרים על ידי שימוש בתכונות מתקדמות של הפונקציה "createTempFile". לדוגמה, אפשר לקבוע את התיקייה בה יישמר הקובץ זמני, כדי להקל על הניתוב ואת גודל הקובץ הזמני.

## ראה גם:

- פירוט של פונקציות Java API: http://clojure-doc.org/articles/language/java_interoperability.html
- הסיוע במחברת של Clojure: https://clojure.org/guides/repl/introduction
- דרכים לנהל קבצים ממוחשבים על ידי שימוש בכתבי מסמכים: https://clojure.github.io/core.matrix/latest/tutorial/documents_management.html

ראה גם:

## ראה גם:

- פירוט של פונקציות Java API: http://clojure-doc.org/articles/language/java_interoper