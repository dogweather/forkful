---
date: 2024-01-20 17:54:44.321015-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4\
  \ \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05D2\u05D9\u05E9\u05D4 \u05DC\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05E9\u05DE\u05D0\u05D5\u05D7\u05E1\u05E0\u05D9\u05DD\
  \ \u05D1\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8. \u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D8\u05E2\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E2\
  \u05D1\u05D3 \u05D0\u05D5\u05EA\u05DD \u05D0\u05D5 \u05DC\u05D4\u05D2\u05D3\u05D9\
  \u05E8 \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05DC\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05D0\u05D7\u05E8\u05D9\u05DD."
lastmod: '2024-03-11T00:14:12.750474-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05DE\
  \u05D1\u05E6\u05E2\u05D9\u05DD \u05D2\u05D9\u05E9\u05D4 \u05DC\u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E9\u05DE\u05D0\u05D5\u05D7\u05E1\u05E0\u05D9\u05DD \u05D1\
  \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8. \u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05D8\u05E2\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E2\u05D1\
  \u05D3 \u05D0\u05D5\u05EA\u05DD \u05D0\u05D5 \u05DC\u05D4\u05D2\u05D3\u05D9\u05E8\
  \ \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05DC\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05D0\u05D7\u05E8\u05D9\u05DD."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## What & Why?
קריאת קובץ טקסט היא פעולה שבה מבצעים גישה לנתונים שמאוחסנים בקובץ טקסט. תכנתים עושים זאת כדי לטעון נתונים, לעבד אותם או להגדיר הגדרות לתכנתים אחרים.

## How to:
### קריאת תכנים מלאים עם readText()
```kotlin
import java.nio.file.Paths

fun main() {
    val path = Paths.get("example.txt")
    val content = path.toFile().readText(Charsets.UTF_8)
    println(content)
}
```
תוצאה:
```
התוכן של הקובץ example.txt נקרא
```

### קריאת שורה אחר שורה עם forEachLine()
```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    Files.newBufferedReader(Paths.get("example.txt")).use { reader ->
        reader.forEachLine { line ->
            println(line)
        }
    }
}
```
תוצאה:
```
שורה ראשונה
שורה שנייה
שורה שלישית
```

## Deep Dive
קריאת קבצים היא חלק מהיסוד לכל שפת תכנות. בעבר, הגישה לקבצים הייתה כרוכה בפתיחה וסגירה ידנית של המשאבים. כיום בקוטלין, מתקיימת דגש על "safe closing" באמצעות use{}. ישנן גם אלטרנטיבות כמו ספריית Apache Commons IO בג'אווה אשר מאפשרות גישה נוחה ומרוכזת יותר לקריאת קבצים. בתוך קוטלין, אפשר להשתמש בstream כדי לקרוא קבצים גדולים באופן יעיל יותר, מבלי לטעון אותם כולם לזיכרון במכה אחת.

## See Also
- [Baeldung Kotlin - Reading a File](https://www.baeldung.com/kotlin/read-file)
- [Apache Commons IO Library](https://commons.apache.org/proper/commons-io/)
