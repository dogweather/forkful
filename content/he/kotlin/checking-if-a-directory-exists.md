---
title:                "לבדיקה האם התיקייה קיימת"
html_title:           "Kotlin: לבדיקה האם התיקייה קיימת"
simple_title:         "לבדיקה האם התיקייה קיימת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

יש רבים וטובים לבדוק אם תיקייה קיימת, כגון לאפשר להתאים את הקוד למיקום שונה או לוודא לפני יצירת תיקייה חדשה.

## איך לבדוק אם תיקייה קיימת בקוד Kotlin

תחילה, יש ליצור פונקציה המקבלת כפרמטר את הנתיב לתיקייה שברצונך לבדוק. לדוגמה:

```Kotlin
fun checkDirectoryExists(path: String) {
    val directory = File(path)
    if (directory.exists()) {
        println("התיקייה קיימת")
    } else {
        println("התיקייה לא קיימת")
    }
}
```

לאחר מכן, ניתן לקרוא לפונקציה עם הנתיב לתיקייה כארגומנט:

```Kotlin
val path = "/home/user/Documents"
checkDirectoryExists(path)
```

תכונת הבדיקה אם תיקייה קיימת ב-Kotlin מופעלת על ידי יצירת אובייקט מסוג File וחיפוש בעזרת הפעולה exists().

## המעמקים של בדיקת קיומה של תיקייה

ב-Kotlin, ניתן לבצע בדיקה של קיומה של תיקייה גם באמצעות תכונת ה-if ובעזרת הפעולה isDirectory מכיוון שתיקייה מסוג File הוא ממשיות של הממשק File.

דוגמה:

```Kotlin
if (File(path).isDirectory) {
    println("זו תיקייה")
} else {
    println("אינה תיקייה")
}
```

ניתן גם להשתמש בפעולת exists() במבני הנתונים של המיבנד ולבצע בדיקה של קיומה של התיקייה:

```Kotlin
val listOfPaths = listOf("/home/user/Documents", "/home/user/Downloads", "/home/user/Pictures")

listOfPaths.forEach{
    if (File(it).exists()) {
        println("$it קיימת")
    } else {
        println("$it לא קיימת")
    }
}
```

## ראו גם

- [תיעוד הפעולה exists() ב-Kotlin] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
- [תמיכה בתיקיות ב-Kotlin] (https://kotlinlang.org/docs/reference/files.html)