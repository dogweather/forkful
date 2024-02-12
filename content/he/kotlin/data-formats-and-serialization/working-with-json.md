---
title:                "עבודה עם JSON"
aliases: - /he/kotlin/working-with-json.md
date:                  2024-02-03T19:23:57.381446-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON (תצוגת אובייקטים של JavaScript) בקוטלין כוללת פענוח ויצירה של נתוני JSON. מתכנתים עושים זאת כדי להחליף נתונים בקלות בין שכבות שונות באפליקציה, או לתקשר עם שירותי רשת, בזכות הפורמט הקליל והקריא של JSON.

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
