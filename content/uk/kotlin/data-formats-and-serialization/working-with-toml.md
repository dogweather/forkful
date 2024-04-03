---
date: 2024-01-26 04:24:22.578801-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0429\u043E\u0431 \u043E\u0431\u0440\u043E\u0431\u0438\u0442\u0438 TOML \u0443\
  \ Kotlin, \u0432\u0438 \u043C\u043E\u0433\u043B\u0438 \u0431 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0443, \u043D\u0430\u043F\u0440\u0438\u043A\
  \u043B\u0430\u0434, `ktoml`. \u0421\u043F\u043E\u0447\u0430\u0442\u043A\u0443 \u0434\
  \u043E\u0434\u0430\u0439\u0442\u0435 \u0437\u0430\u043B\u0435\u0436\u043D\u0456\u0441\
  \u0442\u044C \u0443 \u0432\u0430\u0448 `build.gradle.kts`."
lastmod: '2024-03-13T22:44:49.258093-06:00'
model: gpt-4-0125-preview
summary: "\u0429\u043E\u0431 \u043E\u0431\u0440\u043E\u0431\u0438\u0442\u0438 TOML\
  \ \u0443 Kotlin, \u0432\u0438 \u043C\u043E\u0433\u043B\u0438 \u0431 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443, \u043D\u0430\u043F\u0440\
  \u0438\u043A\u043B\u0430\u0434, `ktoml`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

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
