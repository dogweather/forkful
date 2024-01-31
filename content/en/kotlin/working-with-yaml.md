---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, which stands for "YAML Ain't Markup Language," is a human-readable data serialization standard. Programmers use it to configure software, define data, or set parameters due to its versatility and readability.

## How to:

To work with YAML in Kotlin, you'll typically use a library like `snakeyaml`. Let's jump into how to parse a YAML file:

First, add the dependency in your `build.gradle` file:

```kotlin
implementation("org.yaml:snakeyaml:1.29")
```

Now let's parse a simple YAML file using SnakeYAML:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.InputStream

fun main() {
    val yaml = Yaml()
    val inputStream: InputStream = this::class.java.classLoader.getResourceAsStream("config.yaml")
    val data: Map<String, Any> = yaml.load(inputStream)

    println(data["name"])
    println(data["age"])
}

// Sample contents of config.yaml:
// name: John Doe
// age: 30

// Sample Output:
// John Doe
// 30
```

This code snippet loads a YAML file and prints the values associated with keys `name` and `age`.

## Deep Dive

YAML emerged in the early 2000s to combat the complexity of XML. It offers a simpler syntax, making it preferential for configuration files. Alternatives include JSON, which is more data-oriented and less human-friendly, and TOML, which is somewhat a middle ground. When handling YAML in Kotlin, libraries like `snakeyaml` provide the parsing engine, hooking into your Kotlin code to convert YAML strings into native data structures.

## See Also

- YAML 1.2 Specification: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Kotlin Documentation: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
