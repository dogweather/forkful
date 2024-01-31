---
title:                "Working with TOML"
date:                  2024-01-25T03:39:56.036260-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with TOML"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-toml.md"
---

{{< edit_this_page >}}

## What & Why?
TOML stands for Tom's Obvious, Minimal Language. It's used for configuration files because it's easy to read and write for humans, while still being easy to parse for machines. Devs reach for TOML to avoid the clutter of XML and the trickiness of JSON when slinging configs.

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
