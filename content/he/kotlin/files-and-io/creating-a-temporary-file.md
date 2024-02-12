---
title:                "יצירת קובץ זמני"
aliases:
- /he/kotlin/creating-a-temporary-file/
date:                  2024-01-20T17:41:13.933681-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא שיטה שבה אנחנו יוצרים קובץ שאינו מיועד לשימוש לטווח ארוך. תכניתנים עושים זאת למטרות כמו אחסון נתונים זמני, טעינת הנתונים ממקור המוגדר כלא אמין, או כחלק מבדיקות אוטומטיות בהן צריך לנקות אחריהן.

## איך לעשות:
```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val tempDir = Paths.get(System.getProperty("java.io.tmpdir"))
    val tempFile = Files.createTempFile(tempDir, "myApp_", ".tmp")

    // כתיבת נתונים לקובץ
    Files.writeString(tempFile, "זוהי הודעת דוגמה בקובץ זמני")

    // קריאת הנתונים מהקובץ
    val readText = Files.readString(tempFile)
    println(readText)

    // ניקוי - מחיקת הקובץ הזמני
    Files.deleteIfExists(tempFile)

    // אימות שהקובץ נמחק באמת
    println("הקובץ הזמני נמחק: ${!Files.exists(tempFile)}")
}

// יציאה:
// זוהי הודעת דוגמה בקובץ זמני
// הקובץ הזמני נמחק: true
```

## ניתוח עמוק
בהיסטוריה, קבצים זמניים נוצרו על-ידי כתיבה ידנית למערכת הקבצים, עם התחשבות קפדנית בשמירת על פרטיות ואבטחה. היום, Java provides APIs, כמו `Files.createTempFile`, המפשטות זאת, מטפלות באופן אוטומטי בבחינת שם קובץ ייחודי ומבטיחות שמירה בתיקיה נכונה. אלטרנטיבה היא שימוש בספריות צד שלישי, אך ברוב המקרים אין צורך כפי שכבר קיימת ספריית ה-IO הרבה כוח בג'אווה עצמה. בנוסף, בזמן יצירת קבצים זמניים חשוב לתכנת נקיון עצמי לאחר שהם כבר לא נחוצים, כדי למנוע זרימה של משאבי המערכת.

## ראה גם
- [createTempFile](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#createTempFile(java.nio.file.Path,java.lang.String,java.lang.String,java.nio.file.attribute.FileAttribute...)) בJava Docs
- [Kotlin IO API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/) לפרטים נוספים על ניהול קבצים בקוטלין
