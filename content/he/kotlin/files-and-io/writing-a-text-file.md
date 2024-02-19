---
aliases:
- /he/kotlin/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:56.160520-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D1Kotlin \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\
  \u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D5\u05D4\u05D6\u05E0\u05EA \u05EA\
  \u05D5\u05DB\u05DF \u05D8\u05E7\u05E1\u05D8 \u05D0\u05DC\u05D9\u05D5, \u05DE\u05E9\
  \u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05DC\u05D0\u05D7\u05E1\u05D5\
  \u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D5\u05D2\u05D9\u05DD, \u05D0\
  \u05D5 \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05EA\u05E6\u05D5\u05E8\u05D4. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05D5\u05DC\u05E2\u05D1\
  \u05D3\u2026"
lastmod: 2024-02-18 23:08:52.816182
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8 \u05D1Kotlin \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\
  \u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D5\u05D4\u05D6\u05E0\u05EA \u05EA\u05D5\
  \u05DB\u05DF \u05D8\u05E7\u05E1\u05D8 \u05D0\u05DC\u05D9\u05D5, \u05DE\u05E9\u05D9\
  \u05DE\u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05DC\u05D0\u05D7\u05E1\u05D5\u05DF\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D5\u05D2\u05D9\u05DD, \u05D0\u05D5\
  \ \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05EA\u05E6\u05D5\u05E8\u05D4. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05D5\u05DC\u05E2\u05D1\u05D3\
  \u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה של קובץ טקסט בKotlin כוללת יצירת קובץ והזנת תוכן טקסט אליו, משימה נפוצה לאחסון נתונים, לוגים, או הגדרות תצורה. מתכנתים עושים זאת כדי לשמור ולעבד נתונים מחוץ למרחב הזיכרון הפולטילי, מה שמבטיח שמירת נתונים בין פעילויות.

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
