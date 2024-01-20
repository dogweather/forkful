---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Clojure: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#מה זה ולמה?
בדיקה אם תיקייה קיימת היא פעולה שבה התכנית בודקת האם ספסיףי תיקייה קיים כבר במערכת הקבצים. מתכנתים בוחרים לבדוק את זה למנוע שגיאות כתיבה לתיקייה שלא קיימת ולוודא שהמאמרים מאוחסנים במקום הנכון.

#איך לעשות:
אפשר לבדוק אם תיקייה קיימת באמצעות המשפט `clojure.io/file` שבו Clojure:

```Clojure
(let [dir (clojure.java.io/file "/path/to/directory")]
  (.exists dir))
```
אם התיקייה קיימת, הקוד מחזיר את הערך `true`. אם היא לא קיימת, הקוד מחזיר את הערך `false`.

#צלילה עמוקה:
בדיקה אם תיקייה קיימת היא מטלה פשוטה לשם ולמרות זאת היא שגרתית בתכנות. למרות ש-clojure.io/file מרגיש כמו הדרך ה"נכונה" לבדוק את זה, קיימות גם שיטות אלטרנטיביות. אחת מהן היא לנסות ליצור את התיקייה עם `clojure.io/make-dir`, שמחזיר `nil` אם התיקיה כבר קיימת. אבל, זו איננה המתודה המומלצת, מכיוון שהשימוש בזה יכול להוביל לתוצאות לא צפויות אם קיימת תיקיה עם השם שהוזן.

#ראה גם:
רקע נוסף ומידע מעמיק אפשר למצוא במקורות הבאים:

[Clojure Documentation: clojure.java.io/file](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)

[Stack Overflow: How to check if a directory exists in Clojure?](https://stackoverflow.com/questions/2850203/how-to-check-if-a-directory-exists-in-clojure)

[Oracle Documentation: File I/O in Clojure](https://docs.oracle.com/cd/E88353_01/html/E37843/bgbkf.html)