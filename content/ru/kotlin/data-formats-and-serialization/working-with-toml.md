---
title:                "Работа с TOML"
aliases: - /ru/kotlin/working-with-toml.md
date:                  2024-01-29T00:04:39.487419-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
TOML - это аббревиатура от Tom's Obvious, Minimal Language ("Очевидный Минималистичный Язык Тома"). Он используется для файлов конфигурации, потому что его легко читать и писать для людей, а также легко анализировать для машин. Разработчики выбирают TOML, чтобы избежать путаницы с XML и хитростей JSON при работе с конфигурациями.

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
