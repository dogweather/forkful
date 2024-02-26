---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:27.716481-07:00
description: "YAML, \u0449\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0454 \"YAML\
  \ Ain't Markup Language\" (YAML - \u0446\u0435 \u043D\u0435 \u043C\u043E\u0432\u0430\
  \ \u0440\u043E\u0437\u043C\u0456\u0442\u043A\u0438), \u0454 \u0432\u0438\u0441\u043E\
  \u043A\u043E\u0447\u0438\u0442\u0430\u0431\u0435\u043B\u044C\u043D\u0438\u043C \u0444\
  \u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0441\u0435\u0440\u0456\u0430\u043B\u0456\
  \u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\u043A\u0438\
  \u0439 \u0447\u0430\u0441\u0442\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F\u2026"
lastmod: '2024-02-25T18:49:46.734535-07:00'
model: gpt-4-0125-preview
summary: "YAML, \u0449\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0454 \"YAML Ain't\
  \ Markup Language\" (YAML - \u0446\u0435 \u043D\u0435 \u043C\u043E\u0432\u0430 \u0440\
  \u043E\u0437\u043C\u0456\u0442\u043A\u0438), \u0454 \u0432\u0438\u0441\u043E\u043A\
  \u043E\u0447\u0438\u0442\u0430\u0431\u0435\u043B\u044C\u043D\u0438\u043C \u0444\u043E\
  \u0440\u043C\u0430\u0442\u043E\u043C \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\
  \u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\u043A\u0438\u0439\
  \ \u0447\u0430\u0441\u0442\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
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
