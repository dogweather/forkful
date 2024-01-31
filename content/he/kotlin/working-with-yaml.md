---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט קובץ המשמש לתיעוד הגדרות ונתונים. תוכניתנים עובדים עם YAML כי הוא אינטואיטיבי, קריא ומתאים לקונפיגורציות ונתונים היררכיים.

## איך לעשות:
כדי לעבוד עם YAML ב-Kotlin, אנו נשתמש בספריית  `snakeyaml`. כאן דוגמא לקריאת קובץ YAML:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.InputStream

fun main() {
    val yaml = Yaml()
    val inputStream: InputStream = this::class.java.classLoader.getResourceAsStream("config.yaml")
    val data = yaml.load<Map<String, Any>>(inputStream)
    println(data)
}
```

פלט דוגמא עבור קובץ `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
```

היה יוצא בקונסול:

```kotlin
{database={host=localhost, port=3306}}
```

## עיון מעמיק
YAML, שמתעתיק ל-"YAML Ain't Markup Language", הוא אלטרנטיבה ל-JSON ו-XML. הוא נוצר ב-2001 ונועד להיות אנושי-קריא יותר. קיימות ספריות חלופיות כמו Jackson או kotlinx.serialization שמאפשרות עבודה עם YAML ב-Kotlin.

## ראה גם
- [הדוקומנטציה הרשמית של YAML](https://yaml.org)
- [Jackson library](https://github.com/FasterXML/jackson-dataformats-text/tree/master/yaml)
- [kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
