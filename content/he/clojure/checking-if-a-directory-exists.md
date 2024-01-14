---
title:    "Clojure: האם תיקייה קיימת: בדיקה"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## מדוע

בדיקת קיום תיקייה היא חלק חשוב משימות התיכנות היום יום. באמצעות זה, אנו יכולים לוודא שהתיקייה שאנו מנסים לגשת אליה כבר קיימת ולפעול בהתאם. במאמר זה, אנו נלמד כיצד לבדוק אם תיקייה קיימת בשפת Clojure.

## איך לעשות זאת

הצעד הראשון הוא לייבא את הספרייה שלך שבה נמצאת הפונקציה "file-exists?" כדי לבדוק אם קובץ קיים. בדוגמה הבאה, אנו מייצרים משתנה בשם "my-dir" המכיל את הנתיב של התיקייה שנרצה לבדוק. נשתמש בפונקציה "file-exists?" כדי לבדוק אם התיקייה קיימת או לא ונחזיר את התוצאה באמצעות פונקציית "println".

```Clojure
(ns my-namespace
  (:require [clojure.java.io :refer [file-exists?]]))

(def my-dir "C:\\users\\username\\my-directory")

(println (file-exists? my-dir))
```

פלט: true

במקרה שהתיקייה לא קיימת, נקבל כתוצאה את הבא:

```Clojure
(ns my-namespace
  (:require [clojure.java.io :refer [file-exists?]]))

(def my-dir "C:\\users\\username\\non-existent-directory")

(println (file-exists? my-dir))
```

פלט: false

## מעמקים

הפונקציה "file-exists?" משתמשת בפונקציות המבוססות על Java כדי לבדוק אם התיקייה קיימת. זה משמעותי כי אם התיקייה נמצאת בעולם Java, אף פעם לא נתקל בקובץ או תיקייה המתאימים לנתיב שמועבר כפרמטר לפונקציה.

בנוסף, אם אתם מעוניינים לבדוק אם תיקייה קיימת תחת תיקייה אחרת, ניתן להשתמש בפונקציה "file-seq" כדי לקבל את כל הקבצים והתיקיות שנמצאים בתוך התיקייה המבוקשת. ניתן לשלב את שתי הפונקציות כדי לבדוק אם תיקייה קיימת תחת