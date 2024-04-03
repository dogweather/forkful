---
date: 2024-01-25 03:39:56.036260-07:00
description: 'How to: To handle TOML in Kotlin, you might use a library like `ktoml`.
  First, let''s add the dependency in your `build.gradle.kts`.'
lastmod: '2024-03-13T22:45:00.070134-06:00'
model: gpt-4-1106-preview
summary: To handle TOML in Kotlin, you might use a library like `ktoml`.
title: Working with TOML
weight: 39
---

## How to:
To handle TOML in Kotlin, you might use a library like `ktoml`. First, let's add the dependency in your `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Now, let's parse some TOML:

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

Assuming `config.toml` looks like this:

```toml
[database]
host = "localhost"
port = 5432
```

Sample output would be:

```
Database Host: localhost
Database Port: 5432
```

## Deep Dive
TOML, cooked up by GitHub co-founder Tom Preston-Werner in 2013, aimed to be more straightforward than YAML and more type-safe than JSON. It's become a hit, especially with Rust's `Cargo` and Go's module system. Alternatives? YAML's got more features, JSON translates directly into objects in many coding languages, and there's always good ol' XML. As for implementation, ktoml, under Apache 2.0 license, is a pure Kotlin library and doesn't drag Java libs along, offering DSLs to write TOML too, not just read.

## See Also
- The TOML GitHub: https://github.com/toml-lang/toml
- The ktoml GitHub: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
