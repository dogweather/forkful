---
title:    "Clojure: בדיקת קיום תיקייה במחשב"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# למה

בכדי לבנות תוכניות יותר יעילות ומדויקות, חשוב לברר האם תיקייה קיימת לפני שמתחילים לכתוב קוד. בדיקה זו יכולה להציג שגיאות מוקדמות כדי שנוכל להתמודד איתן בזמן הוגן.

# איך לבדוק אם תיקייה קיימת

```Clojure
;;להגדיר פונקציה שבודקת האם התיקייה קיימת
(defn check-dir-exists [dir-path]
  (.isDirectory (java.io.File. dir-path)))

;;זה יחזיר true אם התיקייה קיימת ושגיאה אחרת
(check-dir-exists "/Users/username/Documents")

;;העלתי תיקייה שלא קיימת כדי להראות את הפלט השגוי
(check-dir-exists "/Users/username/FakeDocuments")
;;שגיאה: FileNotFoundException לא נמצא קובץ או תיקייה בדרך
```

# לעבוד בעומק יותר

אם אנחנו רוצים לעבוד עם תיקייה ספציפית ולא רק לבדוק האם היא קיימת, אפשר להשתמש בפונקציות נוספות כמו `.listFiles` כדי לקבל את רשימת הקבצים בתיקייה או `.mkdirs` כדי ליצור תיקייה חדשה.

# ראו גם

- למדו עוד על פונקציות של תיקיות בקלוג'ור: https://www.clojure.org/reference/java_interop#_java_io_file
- בידור משתמשים בתיקיות בקלוג'ור: http://blogger.ziesemer.com/2010/07/clojure-file-directory-processing.html
- דוגמאות למימוש של בדיקה אם תיקייה קיימת: https://boopathi.in/blog/clojure/2010/10/14/is-directory-exists-in-clojure/