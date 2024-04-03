---
date: 2024-01-26 04:24:11.355594-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD TOML \u05D1-Kotlin, \u05D9\
  \u05D9\u05EA\u05DB\u05DF \u05E9\u05EA\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 \u05DB\u05DE\u05D5 `ktoml`. \u05E8\u05D0\u05E9\u05D9\u05EA\
  , \u05D1\u05D5\u05D0\u05D5 \u05E0\u05D5\u05E1\u05D9\u05E3 \u05D0\u05EA \u05D4\u05EA\
  \u05DC\u05D5\u05EA \u05D1-`build.gradle.kts` \u05E9\u05DC\u05DB\u05DD."
lastmod: '2024-03-13T22:44:39.311449-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD\
  \ TOML \u05D1-Kotlin, \u05D9\u05D9\u05EA\u05DB\u05DF \u05E9\u05EA\u05E9\u05EA\u05DE\
  \u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05DB\u05DE\u05D5 `ktoml`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

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
