---
title:                "בדיקה האם ספרייה קיימת"
html_title:           "Arduino: בדיקה האם ספרייה קיימת"
simple_title:         "בדיקה האם ספרייה קיימת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
בדיקת קיום תיקייה ב-Clojure משמשת לוודא שהפתח לשימוש בקבצים או תיקיות אכן מציאותי. זה חשוב כדי למנוע שגיאות בעת קריאה, כתיבה, או יצירת קבצים.

## How to (איך לעשות:)
קוד Clojure שמראה איך לבדוק אם תיקייה קיימת:

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (.isDirectory (io/file path)))

(println (directory-exists? "/path/to/my/directory")) ; הפלט: true או false, תלוי אם התיקייה קיימת או לא
```

## Deep Dive (צלילה לעומק)
ב-Clojure, בדיקת קיום תיקייה היא פעולה פשוטה, המאפשרת לנו לנהוג בזהירות כשעובדים עם מערכת הקבצים. מספר שפות מתמקדות בפונקציונליות דומה, כמו Python עם הפונקציה `os.path.isdir()` ו-Ruby עם `Dir.exist?()`. ב-Clojure, אנו משתמשים בספריית `clojure.java.io`, שתוכננה לספק ממשק נוח לעבודה עם קבצים ותיקיות. הקוד משתמש ב-Java interop להפעלת ה-fn `.isDirectory` מהקלאס `java.io.File`.

## See Also (ראה גם)
- [Clojure java.io documentation](https://clojuredocs.org/clojure.java.io)
- [Official Java File Documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [ClojureDocs - a community-powered documentation and examples repository for Clojure](https://clojuredocs.org/)
