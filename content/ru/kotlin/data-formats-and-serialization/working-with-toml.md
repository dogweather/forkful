---
aliases:
- /ru/kotlin/working-with-toml/
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:39.487419-07:00
description: "TOML - \u044D\u0442\u043E \u0430\u0431\u0431\u0440\u0435\u0432\u0438\
  \u0430\u0442\u0443\u0440\u0430 \u043E\u0442 Tom's Obvious, Minimal Language (\"\u041E\
  \u0447\u0435\u0432\u0438\u0434\u043D\u044B\u0439 \u041C\u0438\u043D\u0438\u043C\u0430\
  \u043B\u0438\u0441\u0442\u0438\u0447\u043D\u044B\u0439 \u042F\u0437\u044B\u043A\
  \ \u0422\u043E\u043C\u0430\"). \u041E\u043D \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u0435\u0442\u0441\u044F \u0434\u043B\u044F \u0444\u0430\u0439\u043B\
  \u043E\u0432 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\u0438\
  , \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E\u2026"
lastmod: 2024-02-18 23:08:56.972449
model: gpt-4-0125-preview
summary: "TOML - \u044D\u0442\u043E \u0430\u0431\u0431\u0440\u0435\u0432\u0438\u0430\
  \u0442\u0443\u0440\u0430 \u043E\u0442 Tom's Obvious, Minimal Language (\"\u041E\u0447\
  \u0435\u0432\u0438\u0434\u043D\u044B\u0439 \u041C\u0438\u043D\u0438\u043C\u0430\u043B\
  \u0438\u0441\u0442\u0438\u0447\u043D\u044B\u0439 \u042F\u0437\u044B\u043A \u0422\
  \u043E\u043C\u0430\"). \u041E\u043D \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u0442\u0441\u044F \u0434\u043B\u044F \u0444\u0430\u0439\u043B\u043E\u0432\
  \ \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\u0438, \u043F\
  \u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
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
