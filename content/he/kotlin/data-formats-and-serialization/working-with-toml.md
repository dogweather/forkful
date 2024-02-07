---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:24:11.355594-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML הוא ראשי תיבות של Tom's Obvious, Minimal Language. הוא משמש לקבצי קונפיגורציה מכיוון שהוא קל לקריאה ולכתיבה עבור בני אדם, תוך כדי שנותר קל לניתוח עבור מכונות. מפתחים נעזרים ב-TOML כדי להימנע מהבלאגן של XML ומהמכשולים של JSON בעת שליחת קונפיגורציות.

## איך לעשות:
כדי להתמודד עם TOML ב-Kotlin, ייתכן שתשתמש בספרייה כמו `ktoml`. ראשית, בואו נוסיף את התלות ב-`build.gradle.kts` שלכם:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

כעת, בואו ננתח קצת TOML:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("שרת הדאטאבייס: $host")
    println("פורט הדאטאבייס: $port")
}
```

בהנחה ש-`config.toml` נראה כך:

```toml
[database]
host = "localhost"
port = 5432
```

פלט לדוגמא יהיה:

```
שרת הדאטאבייס: localhost
פורט הדאטאבייס: 5432
```

## צלילה עמוקה
TOML, שנוצר על ידי שותף המייסד של GitHub, טום פרסטון-ורנר ב-2013, שאף להיות יותר פשוט מ-YAML ובעל בטיחות טיפוסים גבוהה יותר מ-JS. הוא הפך ללהיט, במיוחד עם מערכת ה-Cargo של Rust ומערכת המודולים של Go. אלטרנטיבות? ל-YAML יש יותר תכונות, JSON מתורגם ישירות לאובייקטים בשפות תכנות רבות, ותמיד יש את ה-XML הישן והטוב. לגבי היישום, ktoml, הנמצאת תחת רישיון Apache 2.0, היא ספרייה טהורה של Kotlin ואינה מושכת אחריה ספריות של Java, ומציעה DSLs לכתיבת TOML, לא רק לקרוא.

## ראה גם
- TOML ב-GitHub: https://github.com/toml-lang/toml
- ktoml ב-GitHub: https://github.com/akuleshov7/ktoml
- TOML לעומת YAML לעומת JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
