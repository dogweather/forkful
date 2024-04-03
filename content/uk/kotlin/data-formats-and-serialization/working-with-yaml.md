---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:27.716481-07:00
description: "\u042F\u043A: Kotlin \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\
  \u043A\u0438 \u0434\u043B\u044F \u0430\u043D\u0430\u043B\u0456\u0437\u0443 \u0442\
  \u0430 \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457\
  \ YAML, \u0430\u043B\u0435 \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438\
  \ \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0456 \u0441\u0442\u043E\u0440\
  \u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438\
  , \u0442\u0430\u043A\u0456 \u044F\u043A `snakeyaml`\u2026"
lastmod: '2024-03-13T22:44:49.253161-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438\
  \ \u0434\u043B\u044F \u0430\u043D\u0430\u043B\u0456\u0437\u0443 \u0442\u0430 \u0441\
  \u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457 YAML, \u0430\u043B\
  \u0435 \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043F\u043E\u043F\
  \u0443\u043B\u044F\u0440\u043D\u0456 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\
  \u0456 \u044F\u043A `snakeyaml` (\u0434\u043B\u044F \u0437\u0430\u0433\u0430\u043B\
  \u044C\u043D\u043E\u0433\u043E \u0430\u043D\u0430\u043B\u0456\u0437\u0443 YAML)\
  \ \u0442\u0430 `kotlinx.serialization` (\u0437 \u0440\u043E\u0437\u0448\u0438\u0440\
  \u0435\u043D\u043D\u044F\u043C \u0444\u043E\u0440\u043C\u0430\u0442\u0443 YAML),\
  \ \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0444\u0430\u0439\
  \u043B\u0430\u043C\u0438 YAML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

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
