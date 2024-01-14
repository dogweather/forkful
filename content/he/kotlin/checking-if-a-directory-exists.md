---
title:                "Kotlin: בדיקת קיום תיקייה"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
כדי לוודא שהתיקייה קיימת ולגשת אליה בקוד.

## איך לבדוק האם תיקייה קיימת
ראשית, נצטרך לייבא את הספרייה של Kotlin שמאפשרת לנו לבדוק קיום תיקיות.
```Kotlin
    import java.io.File
```
אחר כך, נצרף את שם התיקייה שברצוננו לבדוק האם היא קיימת באמצעות פקודת `exists()` שמחזירה ערך בוליאני.
```Kotlin
    val directory = File("path/to/directory")
    if(directory.exists()){
        println("התיקייה קיימת.")
    } else {
        println("התיקייה לא קיימת.")
    }
```
התוצאה תהיה "התיקייה קיימת." אם התיקייה קיימת ו"התיקייה לא קיימת." אם התיקייה לא קיימת.

## חקירה מעמיקה
כדי לבדוק אם תיקייה קיימת במערכת, התוכנית שלנו משתמשת בפקודה של לינוקס בשם `ls` שמראה את כל הפריטים בתיקייה ביחד עם המידע עליהם.
בקוד הגורם לחפש תיקייה במערכת, ואם היא קיימת, התוכנית מביאה מידע עליה באמצעות הפקודה `ls`.
זהו דוגמא לאחת מהשימושים המעניינים יותר של בדיקת קיום תיקיות בקוד.

## ראה גם
- [בדיקת קיום קבצים ב-Kotlin](https://www.geeksforgeeks.org/kotlin-check-if-file-exists/)
- [הספרייה של Kotlin לטיפול בקבצים ותיקיות](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- [פקודת אימות בקוד ב-Kotlin](https://kotlinlang.org/docs/reference/coding-conventions.html#layout-and-formatting)