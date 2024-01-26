---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט זה ליצור או לשנות תוכן בקובץ שאפשר לקרוא אחרי. תוכניתנים עושים את זה לשמור נתונים, הגדרות, ולוגים.

## איך לעשות:
```clojure
(with-open [w (java.io.BufferedWriter. (java.io.FileWriter. "example.txt"))]
  (.write w "היי, זה קובץ בעברית!"))
```
כאשר תבצעו את הקוד הזה, תוכן "example.txt" יהיה: "היי, זה קובץ בעברית!"

```clojure
(spit "example2.txt" "כתיבה נוספת בלי לדרוס, מצוין!" :append true)
```
`spit` מוסיף טקסט לקובץ בלי למחוק אותו.

## טבילה עמוקה
כתיבת קובצי טקסט התפתחה מימי ה-DOS עד היום. יש אלטרנטיבות כמו קובצי XML ו-JSON. בקלוג'ר, כתיבה לקובץ משתמשת לעתים ב-Java interop כי זו פלטפורמת JVM.

## ראו גם
- [Clojure Docs](https://clojure.org/guides/learn/functions)
- [Clojure for the Brave and True](https://www.braveclojure.com/)
