---
title:                "Робота з YAML"
aliases:
- /uk/kotlin/working-with-yaml.md
date:                  2024-02-03T19:26:27.716481-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
YAML, що означає "YAML Ain't Markup Language" (YAML - це не мова розмітки), є високочитабельним форматом серіалізації даних, який часто використовується для файлів конфігурації, зберігання даних та міжпроцесного обміну повідомленнями. Програмісти часто працюють з YAML для управління конфігураціями та налаштуваннями у структурований, проте простий спосіб, отримуючи вигоду від його ясності та простоти на відміну від JSON або XML, коли важлива читабельність.

## Як:
Kotlin не має вбудованої підтримки для аналізу та серіалізації YAML, але ви можете використовувати популярні сторонні бібліотеки, такі як `snakeyaml` (для загального аналізу YAML) та `kotlinx.serialization` (з розширенням формату YAML), для роботи з файлами YAML.

### Використання `snakeyaml`
**Залежність:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Читання YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Зразок використання
fun main() {
    readYaml("config.yaml")
}
```
**Зразок `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Зразок виводу:**
```
{database={host=localhost, port=5432}}
```
### Використання `kotlinx.serialization` з YAML
Спочатку переконайтеся, що ви маєте бібліотеку `kotlinx-serialization` з підходящою бібліотекою підтримки YAML (якщо доступна, оскільки `kotlinx.serialization` переважно націлена на JSON та інші формати безпосередньо).

**Залежність:**
```kotlin
// Для JSON (ілюстративно, перевірте підтримку YAML або альтернативні бібліотеки)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Визначення серіалізованого класу даних:**
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

На жаль, на час написання, пряма підтримка YAML у `kotlinx.serialization` може бути обмеженою або в розвитку. Вам може знадобитися використати проміжне представлення (наприклад, конвертувати YAML у JSON за допомогою `snakeyaml`, а потім аналізувати JSON за допомогою `kotlinx.serialization`) або шукати спільнотні проєкти серіалізації YAML, сумісні з `kotlinx.serialization`.

Для JSON код виглядав би так:
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

Оскільки Kotlin та його екосистема продовжують еволюціонувати, слідкуйте за офіційною документацією та ресурсами спільноти, щоб дізнатися останнє про підтримку YAML та бібліотеки.
