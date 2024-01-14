---
title:    "Clojure: כתיבה של קובץ טקסט"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## למה:

כתיבת קובץ טקסט היא חלק בלתי נפרד מכתיבת קוד בכל שפת תכנות. כשמכין יצירתי לזרום את המחשבה ומחשב את הפתרון, כתיבת קובץ טקסט מאפשרת לנו לארגן את הקוד באופן ברור וקל לקריאה.

## איך לעשות:

```Clojure
(def text-file "my-text-file.txt")

(defn write-text-file [file content]
  (spit file content))

(def contents "This is the content of my text file.")

(write-text-file text-file contents)
```

הקוד זה ייצור קובץ טקסט חדש בשם "my-text-file.txt" ויכתוב בתוכו את התוכן של המשתנה "contents".

מומלץ להשתמש בפונקציה "slurp" כדי לקרוא קבצי טקסט קיימים ולחזור על התהליך כדי לחבר ולערוך אותם.

## נכנסים לעומק:

כדי לשפר את חווית כתיבת הקוד והפניה לקבצי טקסט, ניתן להשתמש בספרייה "clojure.java.io" שמכילה פונקציות נוחות כמו "spit" ו-"slurp". ניתן גם להשתמש בספרייה של מיסמך סטנדרטי של Clojure בשם "clojure.core". עם כלי זה, ניתן לעשות דברים כולל קריאת קבצי טקסט מודפסים ומיובאים, ולהתמודד עם הכתיבה והקריאה של קבצי טקסט מכל מיקום במערכת הקבצים.

## ראה גם:

- דוקומנטציה רשמית של ספריית הכלים "clojure.java.io": https://clojure.github.io/clojure/clojure.java.io-api.html
- למדו עוד על כוחה של שפת תכנות Clojure עם המדריך המקיף הזה: https://www.braveclojure.com/