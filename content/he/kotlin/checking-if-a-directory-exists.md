---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Kotlin: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# בדיקה אם מחיצה קיימת ב-Kotlin
## מה ולמה?
בדיקה אם מחיצה קיימת היא פעולה שבה אנו מבדיקים אם מחיצה במערכת הקבצים של המחשב מתאימה לנתיב שסיפקנו. מתכנתים מבצעים זאת כדי להבטיח שהפקודות הבאות יפעלו כמצופה ולמנוע שגיאות בתהליך.

## איך?
אנחנו משתמשים ב-infra File Class של Kotlin כדי לבדוק אם מחיצה קיימת. להלן דוגמה:

```Kotlin
import java.io.File

fun main() {
    val path: File = File("/path/to/your/directory")
    
    if (path.exists()) {
        println("The directory exists.")
    } else {
        println("The directory does not exist.")
    }
}
```

זה מסיים את בדיקת התיקייה.

## צלילה עמוקה
1. הקשר ההיסטורי: Kotlin מסייעת למתכנתים לטפל בתיקיות באפליקציות מחשב שלהם באמצעות מחלקות שחולקות בפתרונות Java.
2. חלופות: ניתן בפונקציה `java.nio.file.Files.exists()` לבדיקת קיום מחיצה:
```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/path/to/your/directory")

    if (Files.exists(path)) {
        println("The directory exists.")
    } else {
        println("The directory does not exist.")
    }
}
```
3. פרטי המימוש: נתיבים רשאים להיות נתיבים מוחלטים או נתיבים יחסיים למחיצה.

## ראו גם
1. המסמכים של Kotlin על אובייקט File: [כאן](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/)
2. בדיקת קיום של קובץ או תיקייה ב- Kotlin: [כאן](https://www.baeldung.com/kotlin/file-exists)
3. המסמכים של Java על האובייקט Paths: [כאן](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Paths.html)