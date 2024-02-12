---
title:                "Working with YAML"
aliases:
- /en/kotlin/working-with-yaml.md
date:                  2024-02-03T19:03:28.960635-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, which stands for YAML Ain't Markup Language, is a highly readable data serialization format often used for configuration files, data storage, and inter-process messaging. Programmers frequently work with YAML to manage configurations and settings in a structured yet straightforward manner, benefiting from its clarity and simplicity over JSON or XML when readability matters.

## How to:
Kotlin does not have built-in support for YAML parsing and serialization, but you can utilize popular third-party libraries such as `snakeyaml` (for general YAML parsing) and `kotlinx.serialization` (with a YAML format extension) to work with YAML files.

### Using `snakeyaml`
**Dependency:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Read YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Sample usage
fun main() {
    readYaml("config.yaml")
}
```
**Sample `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Sample Output:**
```
{database={host=localhost, port=5432}}
```
### Using `kotlinx.serialization` with YAML
First, ensure you have the `kotlinx-serialization` library with a suitable YAML support library (if available, as `kotlinx.serialization` primarily targets JSON and other formats directly).

**Dependency:**
```kotlin
// For JSON (illustrative, check for YAML support or alternative libraries)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Define a serializable data class:**
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

Unfortunately, at the time of writing, direct YAML support in `kotlinx.serialization` might be limited or evolving. You may need to use an intermediate representation (such as converting YAML to JSON with `snakeyaml` and then parsing JSON with `kotlinx.serialization`) or look for community-driven YAML serialization projects compatible with `kotlinx.serialization`.

For JSON, the code would look something like this:
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

As Kotlin and its ecosystem continue to evolve, keep an eye on the official documentation and community resources for the latest in YAML support and libraries.
