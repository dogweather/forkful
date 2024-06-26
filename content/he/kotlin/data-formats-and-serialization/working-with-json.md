---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:57.381446-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E7\u05D5\u05D8\
  \u05DC\u05D9\u05DF \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\
  \u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-JSON \u05D0\
  \u05DA \u05D4\u05D9\u05D0 \u05DE\u05E0\u05E6\u05DC\u05EA \u05D0\u05EA \u05D4\u05EA\
  \u05DB\u05D5\u05E0\u05D5\u05EA \u05D4\u05D7\u05D6\u05E7\u05D5\u05EA \u05E9\u05DC\
  \ \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9\
  \ \u05DB\u05D2\u05D5\u05DF `Gson` \u05DE\u05D1\u05D9\u05EA \u05D2\u05D5\u05D2\u05DC\
  \ \u05D5-`Kotlinx.serialization` \u05DE\u05D1\u05D9\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.308282-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05D5\u05D8\u05DC\u05D9\u05DF \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\
  \u05EA \u05DC-JSON \u05D0\u05DA \u05D4\u05D9\u05D0 \u05DE\u05E0\u05E6\u05DC\u05EA\
  \ \u05D0\u05EA \u05D4\u05EA\u05DB\u05D5\u05E0\u05D5\u05EA \u05D4\u05D7\u05D6\u05E7\
  \u05D5\u05EA \u05E9\u05DC \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\
  \u05DC\u05D9\u05E9\u05D9 \u05DB\u05D2\u05D5\u05DF `Gson` \u05DE\u05D1\u05D9\u05EA\
  \ \u05D2\u05D5\u05D2\u05DC \u05D5-`Kotlinx.serialization` \u05DE\u05D1\u05D9\u05EA\
  \ JetBrains."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## איך לעשות:
קוטלין אינה כוללת תמיכה מובנית ל-JSON אך היא מנצלת את התכונות החזקות של ספריות צד שלישי כגון `Gson` מבית גוגל ו-`Kotlinx.serialization` מבית JetBrains. הנה איך אתם יכולים להשתמש בשניהם לעבודה עם JSON.

### שימוש ב-Gson
הוספת התלות של Gson לקובץ `build.gradle` שלך:
```kotlin
implementation 'com.google.code.gson:gson:2.8.9'
```

פענוח מחרוזת JSON לאובייקט ולהיפך:
```kotlin
import com.google.gson.Gson

// הגדרת מחלקת נתונים
data class User(val name: String, val age: Int)

fun main() {
    val gson = Gson()

    // סריאליזציה
    val json = gson.toJson(User("John Doe", 30))
    println(json)  // פלט: {"name":"John Doe","age":30}

    // דיסריאליזציה
    val user: User = gson.fromJson(json, User::class.java)
    println(user)  // פלט: User(name=John Doe, age=30)
}
```

### שימוש ב-Kotlinx.serialization
ראשית, כלול את התלות ב-`build.gradle` שלך:
```kotlin
implementation "org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.3"
```

לאחר מכן, החל את תוסף `kotlinx-serialization` בראש סקריפט הבנייה שלך:
```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    kotlin("plugin.serialization") version "1.6.10"
}
```

סריאליזציה ודיסריאליזציה עם Kotlinx.serialization:
```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// הגדרת מחלקת נתונים סריאליזבילית
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // סריאליזציה
    val json = Json.encodeToString(User("Jane Doe", 28))
    println(json)  // פלט: {"name":"Jane Doe","age":28}

    // דיסריאליזציה
    val user = Json.decodeFromString<User>(json)
    println(user)  // פלט: User(name=Jane Doe, age=28)
}
```

גם Gson וגם Kotlinx.serialization מפשטים את העבודה עם JSON באפליקציות קוטלין, הבחירה באחת על פני השנייה תלויה בדרישות הפרויקט הספציפיות שלכם ובהעדפות האישיות.
