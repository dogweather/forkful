---
title:                "כתיבת קובץ טקסט"
html_title:           "Kotlin: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה
מחברת קודות בקוטלין היא הדרך המושלמת לכתיבת קובץ טקסט עבור מטרות חדשות ומגוונות. זה מאפשר לנו ליצור קבצים עם תוכן דינמי ולעבוד איתם בקלות מבלי שצריך להשתמש בתוכנות חיצוניות.

## איך לעשות זאת
הנה דוגמה לכיצד ניתן לכתוב קובץ טקסט בפורמט Markdown באמצעות קודות קוטלין:

```
fun main() {
    val filePath = "./myfile.txt"
    val content = "זהו דוגמה למחברת קודות בקוטלין"
    File(filePath).writeText(content)
}
```
הקוד הזה יייצר קובץ בשם "myfile.txt" בתיקייה הנוכחית ויכתוב בתוכו את התוכן "זהו דוגמה למחברת קודות בקוטלין". ניתן לשנות את המסלול של הקובץ ואת התוכן לפי הצורך המיוחד שלנו.

## Deep Dive
כמו שראינו בדוגמה, כתיבת קובץ טקסט בקוטלין דורשת מאיתנו שתי פעולות עיקריות: ליצור אובייקט של File עם המסלול של הקובץ שאנחנו רוצים ליצור, ולהשתמש בפונקציה writeText() כדי לכתוב את התוכן שנרצה לכתוב. כמו כן, ניתן להשתמש במגוון רחב של פונקציות נוספות לכתיבת קבצים בקוטלין, כגון writeLines() לכתיבת רשימת מחרוזות לקובץ בכמה שורות ו appendText() להוספת תוכן לקובץ קיים.

## ראו גם
- Kotlin תיעוד רשמי: https://kotlinlang.org/docs/reference/input-output.html#writing-text-files
- דוגמאות נוספות של כתיבת קבצים בקוטלין: https://www.geeksforgeeks.org/write-a-file-in-kotlin/
- תרגול על כתיבת קבצים בקוטלין בעזרת השיעורים: https://kotlinlang.org/docs/reference/basic-types.html#strings