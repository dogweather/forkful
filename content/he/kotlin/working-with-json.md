---
title:                "עובדים עם json"
html_title:           "Kotlin: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## למה

ישנם הרבה סיבות לכתוב קוד ב-Kotlin, ושימוש ב-JSON נחשב לאחת המפתחות. JSON הוא פורמט נתונים נפוץ ופשוט לשימוש, ויכול לשמש כחלק משמעותי בכל יישום שמתמקד בהעברת מידע בין שרת ולקוח.

## איך לעשות זאת

כדי לכתוב קוד ב-Kotlin שמשתמש ב-JSON, נצטרך להתחבר לספריית gson הכותרית. נתחיל עם בניית מחלקה שתייצג את הנתונים שנרצה לקבל מקובץ JSON.

```Kotlin
class Person(
    val name: String,
    val age: Int,
    val profession: String
)
```

לאחר מכן, נגדיר משתנה שמכיל את הנתונים הרלוונטים בפורמט JSON.

```Kotlin
val json = """
    {
        "name": "John",
        "age": 30,
        "profession": "Developer"
    }
""".trimIndent()
```

בסוף, נשתמש בפונקציית Gson של הספרייה להמיר את המחרוזת שלנו לאובייקט של מחלקת Person.

```Kotlin
val person = Gson().fromJson(json, Person::class.java)
println("Name: ${person.name}, Age: ${person.age}, Profession: ${person.profession}")

output: 
Name: John, Age: 30, Profession: Developer
```

## כיצד לעמום עומק

הרי כבר מהכירים את כלי ה-Gson של הספרייה, אך ייתכן שיהיו לכם צרכים מתקדמים מאוד ותרצו להעמיק. נתייחס עכשיו לשתי נסיבות אפשריות שנדון בהן: טיפוסים מותאמים אישית והתייחסות לנתונים נתונים בפנייה.

### טיפוסים מותאמים אישית

ספריית gson נותנת לנו אפשרות להשתמש בכיתות מותאמות אישית במקום הכיתות המוכרות, כדי להתאים את הנתונים לטווחי ערכים או לנתונים שאנחנו צריכים.

```Kotlin
class CustomSerializer : JsonSerializer<LocalDateTime> {
    override fun serialize(src: LocalDateTime, typeOfSrc: Type, context: JsonSerializationContext): JsonElement {
        return JsonPrimitive(src.toInstant(ZoneOffset.UTC).toEpochMilli())
    }
}

val gson = GsonBuilder()
    .registerTypeAdapter(LocalDateTime::class.java, CustomSerializer())
    .create()

val date = LocalDateTime.now()

println(gson.toJson(date))

ouput