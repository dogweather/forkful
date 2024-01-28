---
title:                "Робота з TOML"
date:                  2024-01-26T04:24:22.578801-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і чому?
TOML - це абревіатура Tom's Obvious, Minimal Language (Очевидна, мінімалістична мова Тома). Вона використовується для файлів налаштувань тому, що її легко читати і писати людям, при тому вона також легко аналізується машинами. Розробники вибирають TOML, щоб уникнути нагромаджень XML та плутанини JSON при роботі з конфігураціями.

## Як це зробити:
Щоб обробити TOML у Kotlin, ви могли б використовувати бібліотеку, наприклад, `ktoml`. Спочатку додайте залежність у ваш `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Тепер давайте розпарсимо деякий TOML:

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

Припускаючи, що `config.toml` виглядатиме так:

```toml
[database]
host = "localhost"
port = 5432
```

Приклад виводу буде:

```
Database Host: localhost
Database Port: 5432
```

## Поглиблено
TOML, створений співзасновником GitHub Томом Престон-Вернером у 2013 році, мав на меті бути простішим за YAML і більш типобезпечним за JSON. Він став дуже популярним, особливо з `Cargo` у Rust і системою модулів у Go. Альтернативи? YAML має більше функцій, JSON безпосередньо перетворюється на об'єкти в багатьох мовах програмування, і завжди є добрий старий XML. Що стосується реалізації, ktoml під ліцензією Apache 2.0 є чисто Kotlin бібліотекою і не тягне за собою Java бібліотеки, пропонуючи DSL для написання TOML теж, а не тільки для читання.

## Див. також
- GitHub TOML: https://github.com/toml-lang/toml
- GitHub ktoml: https://github.com/akuleshov7/ktoml
- TOML проти YAML проти JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
