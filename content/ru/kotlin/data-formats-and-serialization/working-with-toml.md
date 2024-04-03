---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:39.487419-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0435\
  \u0442\u0441\u044F: \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\
  \u0430\u0442\u044C \u0441 TOML \u0432 Kotlin, \u0432\u044B \u043C\u043E\u0436\u0435\
  \u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443, \u043D\u0430\u043F\
  \u0440\u0438\u043C\u0435\u0440, `ktoml`. \u0414\u043B\u044F \u043D\u0430\u0447\u0430\
  \u043B\u0430 \u0434\u043E\u0431\u0430\u0432\u0438\u043C \u0437\u0430\u0432\u0438\
  \u0441\u0438\u043C\u043E\u0441\u0442\u044C \u0432 \u0432\u0430\u0448\u2026"
lastmod: '2024-03-13T22:44:45.021639-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0442\
  \u044C \u0441 TOML \u0432 Kotlin, \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443, \u043D\u0430\u043F\u0440\
  \u0438\u043C\u0435\u0440, `ktoml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

## Как это делается:
Чтобы работать с TOML в Kotlin, вы можете использовать библиотеку, например, `ktoml`. Для начала добавим зависимость в ваш `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Теперь давайте разберем TOML:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("Database Host: $host")
    println("Database Port: $port")
}
```

Предполагая, что `config.toml` выглядит так:

```toml
[database]
host = "localhost"
port = 5432
```

Пример вывода будет:

```
Database Host: localhost
Database Port: 5432
```

## Глубокое погружение
TOML, созданный в 2013 году соучредителем GitHub Томом Престон-Вернером, задумывался быть более простым, чем YAML, и более типобезопасным, чем JSON. Он стал популярным, особенно с системой модулей Rust `Cargo` и системой модулей Go. Альтернативы? У YAML больше функций, JSON напрямую переводится в объекты во многих языках программирования, а также всегда есть добрый старый XML. Что касается реализации, ktoml, под лицензией Apache 2.0, является чистой библиотекой Kotlin и не тянет за собой библиотеки Java, предлагая DSL для записи TOML, а не только для чтения.

## Смотрите также
- GitHub TOML: https://github.com/toml-lang/toml
- GitHub ktoml: https://github.com/akuleshov7/ktoml
- Сравнение TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
