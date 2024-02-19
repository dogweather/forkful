---
aliases:
- /he/kotlin/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:32.636087-07:00
description: "YAML, \u05E9\u05DE\u05E9\u05DE\u05E2\u05D5\u05EA\u05D5 YAML Ain't Markup\
  \ Language, \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E1\u05E8\u05D9\
  \u05D0\u05DC\u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05E9\u05E7\u05E8\u05D9\u05D0 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D5\
  \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\
  \u05E9\u05DE\u05E9 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4\
  , \u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D5\u05D4\
  \u05D5\u05D3\u05E2\u05D5\u05EA \u05D1\u05D9\u05DF-\u05EA\u05D4\u05DC\u05D9\u05DB\
  \u05D9\u05D5\u05EA.\u2026"
lastmod: 2024-02-18 23:08:52.818510
model: gpt-4-0125-preview
summary: "YAML, \u05E9\u05DE\u05E9\u05DE\u05E2\u05D5\u05EA\u05D5 YAML Ain't Markup\
  \ Language, \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E1\u05E8\u05D9\
  \u05D0\u05DC\u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05E9\u05E7\u05E8\u05D9\u05D0 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D5\
  \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\
  \u05E9\u05DE\u05E9 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4\
  , \u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D5\u05D4\
  \u05D5\u05D3\u05E2\u05D5\u05EA \u05D1\u05D9\u05DF-\u05EA\u05D4\u05DC\u05D9\u05DB\
  \u05D9\u05D5\u05EA.\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
---

{{< edit_this_page >}}

## מה ולמה?
YAML, שמשמעותו YAML Ain't Markup Language, הוא פורמט סריאליזציה של נתונים שקריא ביותר ולעיתים קרובות משמש לקבצי תצורה, אחסון נתונים, והודעות בין-תהליכיות. מתכנתים לעיתים קרובות עובדים עם YAML כדי לנהל תצורות והגדרות באופן מובנה אך פשוט, תוך ניצול הבירור והפשטות שלו על פני JSON או XML כאשר הקריאות חשובה.

## איך לעשות זאת:
Kotlin אין לו תמיכה מובנית לפירוז וסריאליזציה של YAML, אבל אתם יכולים להשתמש בספריות צד שלישי פופולריות כמו `snakeyaml` (לפירוז כללי של YAML) ו-`kotlinx.serialization` (עם הרחבת פורמט YAML) כדי לעבוד עם קבצי YAML.

### שימוש ב-`snakeyaml`
**תלות:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**קריאת YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// דוגמא לשימוש
fun main() {
    readYaml("config.yaml")
}
```
**דוגמא ל-`config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**דוגמא לפלט:**
```
{database={host=localhost, port=5432}}
```
### שימוש ב-`kotlinx.serialization` עם YAML
ראשית, ודאו שיש לכם את ספריית `kotlinx-serialization` עם ספריית תמיכה ב-YAML מתאימה (אם זמינה, מאחר ש-`kotlinx.serialization` מכוון בעיקר ל-JSON ופורמטים אחרים באופן ישיר).

**תלות:**
```kotlin
// ל-JSON (לדוגמה, בדקו תמיכה ב-YAML או ספריות חלופיות)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**הגדרת מחלקת נתונים סריאלית:**
```kotlin
import kotlinx.serialization.Serializable

@Serializable
data class Config(
    val database: Database
)

@Serializable
data class Database(
    val host: String,
    val port: Int
)
```

לצערנו, נכון לזמן כתיבת שורות אלו, תמיכה ישירה ב-YAML ב-`kotlinx.serialization` עשויה להיות מוגבלת או בתהליך שינוי. ייתכן שתצטרכו להשתמש בייצוג ביניים (כמו המרת YAML ל-JSON עם `snakeyaml` ולאחר מכן פירוז JSON עם `kotlinx.serialization`) או לחפש פרויקטים של צד שלישי בנושא סריאליזציה של YAML התואמים ל-`kotlinx.serialization`.

עבור JSON, הקוד יראה כך:
```kotlin
import kotlinx.serialization.json.Json
import kotlinx.serialization.decodeFromString

fun main() {
    val jsonText = """
    {
        "database": {
            "host": "localhost",
            "port": 5432
        }
    }
    """.trimIndent()
    
    val config = Json.decodeFromString<Config>(jsonText)
    println(config)
}
```

ככל ש-Kotlin ואקוסיסטם שלו ממשיכים להתפתח, שימרו עין פקוחה על הדוקומנטציה הרשמית ומשאבים בקהילה לאחרונות בתמיכה וספריות של YAML.
