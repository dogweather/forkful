---
title:                "Kotlin: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

בתוך תחום התכנות, נתקלים לעיתים קרובות בצורך לקרוא ולהעביר מידע מכל סוגים שונים. פעמים רבות, נתקלים בסוג של מידע מסוגים מתוארים בפורמט פשטותי יותר משאר הפורמטים הקיימים, כגון לדוגמה קבצי YAML. בטקסט הבא, נתייחס לסיבות מדוע כדאי לעבוד עם פורמט זה.

## איך לעבוד עם YAML בקוטלין?

ראשית נצור קובץ פשוט בשם `example.yaml` ונכתוב בתוכו את המידע הבא:

```Kotlin
name: "John Smith"
age: 30
hobbies:
  - reading
  - cooking
  - hiking
```

כעת, נשתמש בקוד הבא כדי לקרוא את המידע מהקובץ ולהדפיס אותו בצורה מסודרת:

```Kotlin
import org.yaml.snakeyaml.Yaml

fun main() {
    val stream = this::class.java.getResourceAsStream("example.yaml")
    val yaml = Yaml().load<Map<String, Any>>(stream)

    val name = yaml["name"] as String
    val age = yaml["age"] as Int
    val hobbies = yaml["hobbies"] as List<*>

    println("Name: $name")
    println("Age: $age")
    println("Hobbies:")
    hobbies.forEach { println(" - $it") }
}
```

פלט התוכנית הוא:

```
Name: John Smith
Age: 30
Hobbies:
 - reading
 - cooking
 - hiking
```

עם קוד קצר כמו בדוגמה שלנו, כבר אפשר לראות את היתרונות של YAML: קריאות גבוהה, פשטות ליצירת ולעדכן קבצים וגם חסינות מפני שגיאות שמות וסימני פיסוק לפרטים נוספים נעבור לסעיף הבא.

## כניסה מעמיקה

הכניסה מעמיקה לפרמטרים הנתמכים בפורמט YAML עומדת בפני פלטת יחסים מאוד רחבה כמו גם פלט יחסים ערוכה. לכן, הוא נעשה כיווץ תוך כדי ניסוח בפקודת שורה אחת כגון זו:

```
options: !!com.example.ConfigOptions
  initialize:
    enabled: true
    max-retries: 3
  retries:
    connect: 1
    read: 2
    timeout: 10000
```

מכאן נתקל