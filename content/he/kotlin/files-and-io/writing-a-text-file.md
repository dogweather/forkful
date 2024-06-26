---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:56.160520-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Kotlin \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\
  \u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D1\u05E6\u05D9\u05DD, \u05EA\u05D5\
  \u05DA \u05E0\u05D9\u05E6\u05D5\u05DC \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4\
  \ \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05DC\u05DC\u05D0 \u05E6\
  \u05D5\u05E8\u05DA \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\
  \u05DC\u05D9\u05E9\u05D9 \u05E0\u05D5\u05E1\u05E4\u05D5\u05EA. \u05DC\u05D4\u05DC\
  \u05DF \u05D3\u05D5\u05D2\u05DE\u05D0 \u05E4\u05E9\u05D5\u05D8\u05D4."
lastmod: '2024-03-13T22:44:39.302736-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u05DE\u05E1\u05E4\u05E7\u05EA \u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\
  \u05D9\u05E8\u05D4 \u05DC\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D1\u05E6\
  \u05D9\u05DD, \u05EA\u05D5\u05DA \u05E0\u05D9\u05E6\u05D5\u05DC \u05D4\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA\
  \ \u05DC\u05DC\u05D0 \u05E6\u05D5\u05E8\u05DA \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\
  \u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05E0\u05D5\u05E1\u05E4\u05D5\
  \u05EA."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## איך לעשות:
Kotlin מספקת גישה ישירה לכתיבה לקבצים, תוך ניצול הספרייה הסטנדרטית ללא צורך בספריות צד שלישי נוספות. להלן דוגמא פשוטה:

```kotlin
import java.io.File

fun main() {
    val textToWrite = "Hello, Kotlin file writing!"
    File("example.txt").writeText(textToWrite)
}
```
קטע הקוד הזה יוצר קובץ בשם "example.txt" בתיקיית השורש של הפרויקט וכותב את המחרוזת `Hello, Kotlin file writing!` אליו. אם הקובץ כבר קיים, הוא יוחלף.

לכתיבה מבוקרת יותר לקובץ או לכתיבת כמויות גדולות יותר של נתונים, תוכל להשתמש ב`appendText` או ב`bufferedWriter()`:

```kotlin
import java.io.File

fun appendToFile() {
    val moreText = "Appending more text."
    File("example.txt").appendText(moreText)
}

fun writeWithBufferedWriter() {
    val largeText = "Large amounts of text...\nOn multiple lines."
    File("output.txt").bufferedWriter().use { out ->
        out.write(largeText)
    }
}

fun main() {
    appendToFile() // מוסיף טקסט לקובץ הקיים
    writeWithBufferedWriter() // כותב נתוני טקסט גדולים ביעילות
}
```

בפונקציית ה`appendToFile`, אנו מוסיפים טקסט נוסף ל"example.txt" בלי למחוק את התוכן הנוכחי שלו. פונקציית ה`writeWithBufferedWriter` מציגה דרך יעילה לכתיבת כמויות גדולות של טקסט או נתונים, שימושי במיוחד למזעור פעולות ק/כ (I/O) כשמתמודדים עם מספר שורות או קבצים גדולים.

דוגמאות אלו מכסות את הפעולות הבסיסיות לכתיבת קבצי טקסט בKotlin, מדגימות את הפשטות והעוצמה של ספריית הסטנדרט של Kotlin עבור פעולות ק/כ בקבצים.
