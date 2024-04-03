---
date: 2024-01-26 04:24:22.578801-07:00
description: "TOML - \u0446\u0435 \u0430\u0431\u0440\u0435\u0432\u0456\u0430\u0442\
  \u0443\u0440\u0430 Tom's Obvious, Minimal Language (\u041E\u0447\u0435\u0432\u0438\
  \u0434\u043D\u0430, \u043C\u0456\u043D\u0456\u043C\u0430\u043B\u0456\u0441\u0442\
  \u0438\u0447\u043D\u0430 \u043C\u043E\u0432\u0430 \u0422\u043E\u043C\u0430). \u0412\
  \u043E\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0454\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u0444\u0430\u0439\u043B\u0456\
  \u0432 \u043D\u0430\u043B\u0430\u0448\u0442\u0443\u0432\u0430\u043D\u044C \u0442\
  \u043E\u043C\u0443, \u0449\u043E \u0457\u0457 \u043B\u0435\u0433\u043A\u043E\u2026"
lastmod: '2024-03-13T22:44:49.258093-06:00'
model: gpt-4-0125-preview
summary: "TOML - \u0446\u0435 \u0430\u0431\u0440\u0435\u0432\u0456\u0430\u0442\u0443\
  \u0440\u0430 Tom's Obvious, Minimal Language (\u041E\u0447\u0435\u0432\u0438\u0434\
  \u043D\u0430, \u043C\u0456\u043D\u0456\u043C\u0430\u043B\u0456\u0441\u0442\u0438\
  \u0447\u043D\u0430 \u043C\u043E\u0432\u0430 \u0422\u043E\u043C\u0430)."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

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
