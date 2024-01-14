---
title:                "Clojure: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כתיבת קובץ טקסט בקלוז'רה היא דרך יעילה וייצוגית ליצירת קוד ולתיעודו. בדף זה תלמדו איך לכתוב קובץ טקסט מתוך קוד קלוז'ר.

## איך לעשות זאת

```Clojure
  (defn write-file [file-name text]
    "A function that takes in a file name and text as parameters and writes the text into the file. Returns true if successful, false if not."
    (try
      (with-open [writer (clojure.java.io/writer file-name :append true)]
        (.write writer text))
        true
      (catch Exception e
        (println "Error:" (.getMessage e))
        false)))
```

כתיבת פונקציה בשם "write-file" שמקבלת שם קובץ וטקסט כפרמטרים ומכתיבה את הטקסט לקובץ. הפונקציה מחזירה אמת אם התהליך הצליח או שקר אם נכשל. יש כאן שימוש ב-"try-catch" בכדי לטפל בשגיאות אם ישנן.

```Clojure
  (def file-name "my-file.txt")
  (def text "קוד קלוז'רה הוא שפה פונקציונלית ידידותית ויעילה")
  (write-file file-name text)
```

הקוד הנ"ל ייכתב לקובץ בשם "my-file.txt" ויכיל את הטקסט "קוד קלוז'רה הוא שפה פונקציונלית ידידותית ויעילה". אם הפונקציה תצליח, תודפס אמת בטרמינל, ואם כן, אז יש לאחזר משתנה בכל עת שתרצו להשתמש בלולאות.

## Deep Dive

כאשר מבינים איך לכתוב קוד קלוז'רה, יתכן שתרצו לדעת עוד פרטים על כתיבת קבנךבך טקסט. הנה מספר משאבים שיכולים לעזור לכם ללמוד עוד:

- [ספר המדריך הרשמי של קלוז'רה](https://clojure.org/guides/getting_started)
- [אתר הקהילה של קלוז'רה בעברית](https://he.cljdoc.org/)
- [מדריך לכתיבת קוד יעיל וברור בקלוז'רה](https://purelyfunctional.tv/guide/reduce-side-effects-with-clojure/) 

## ראו גם

- [פונקציות ב