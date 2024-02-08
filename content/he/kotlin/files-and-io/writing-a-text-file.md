---
title:                "כתיבת קובץ טקסט"
date:                  2024-02-03T19:28:56.160520-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת קובץ טקסט"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
