---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא פעולה המאפשרת למתכנת להחזיק במידע זמני שאינו נדרש לאחר ריצת התוכנה. אנחנו משתמשים בקבצים זמניים כשאנחנו רוצים להפחית את הזכרון שהתוכנה שלנו משתמשת בו ו/או במהלך תהליכים של ניתוח מידע בחשבון השרת.

## איך ליצור:
קלט Clojure שלך יכול לראות משהו כמו זה:

```Clojure
(import 'java.io.File)
(import 'java.nio.file.Files)
(import 'java.nio.file.StandardCopyOption)

(defn create-temp-file []
  (let [temp-file (File/createTempFile "prefix" "suffix")]
    (println (.getPath temp-file))
    temp-file))
```

זה פשוט ייצר קובץ זמני והדפיס את הנתיב לקונסולה. פלט דוגמתי יכול להיראות כך:

```
/tmp/prefix1234567890suffix
```

## צלילה עמוקה
Clojure היא לוקחת את ההשראה שלה מ-LISP, שולחת התכנות שהתפתחה בשנות ה-50. יצירת קבצי זמני היא חלק קריטי של העיבוד המוני של המידע (Big Data).

לחלופין, אם עבודתך משתמשת רק במעט מאוד מידע, אתה יכול להשתמש גם בסביבת JVM שלך על מנת לאחסן מידע זמני, זה יחסוך לך את הצורך בקבצים זמניים.

התירגול שלנו ממש משתמש בממשק התכנותנית של Java ליצירת קובצים זמניים (java.io.File). זה הופך את העסק הרביד לקובץ זמני פשוט למדי.

## ראה גם
- התיעוד הרשמי של java.io.File: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- מדריך נוסף ליצירת קובץ זמני של Clojure: https://lupan.pl/clj-file-temp/
- מדריך לחסימת IO של stackoverflow עבודה עם קבצים: https://stackoverflow.com/questions/4853924/efficient-file-io-in-clojure