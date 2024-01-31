---
title:                "עבודה עם JSON"
date:                  2024-01-19
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON, שמסמל JavaScript Object Notation, הוא פורמט להחלפת נתונים, קל לקריאה עבור אנשים וקל לעיבוד עבור מחשבים. מתכנתים עובדים עם JSON כי הוא הפורמט המועדף לשליחת וקבלת נתונים ב-APIs וביישומי רשת.

## איך ל:
```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = Json
    val data = User("דוד", 30)

    // קידוד ל-JSON
    val jsonString = json.encodeToString(data)
    println(jsonString) // {"name":"דוד","age":30}

    // פענוח מ-JSON
    val obj = json.decodeFromString<User>(jsonString)
    println(obj) // User(name=דוד, age=30)
}
```

## הצצה לעומק
JSON נוצר בתחילת שנות ה-2000 כחלופה קלת משקל ל-XML. יש חלופות כמו TOML או YAML, אבל JSON נהיה סטנדרט דה-פקטו ברוב המקרים. ב-Kotlin, ניתן לעבוד עם JSON באמצעות ספריית kotlinx.serialization כדי להמיר אובייקטים ל-JSON וחזרה בצורה פשוטה.

## ראו גם
- [Kotlinx Serialization](https://github.com/Kotlin/kotlinx.serialization) - ספריית ה-serialization של Kotlin.
- [Moshi](https://github.com/square/moshi) - ספריית JSON אחרת ל-Kotlin.
- [Gson](https://github.com/google/gson) - ספריית Google לעבודה עם JSON ב-Java ו-Kotlin.
