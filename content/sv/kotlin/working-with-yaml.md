---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML är ett textbaserat format för datakonfiguration. Programmerare använder det för dess läsbarhet och enkelhet vid konfiguration av program och utvecklingsmiljöer.

## How to:
För att hantera YAML i Kotlin behöver vi ett bibliotek som 'snakeyaml'. Lägg till det i ditt `build.gradle`:

```groovy
dependencies {
    implementation("org.yaml:snakeyaml:1.29")
}
```

Nedan är exempel på hur man läser och skriver YAML-data.

Läs YAML:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.InputStream

fun main() {
    val yaml = Yaml()
    val inputStream: InputStream = this::class.java.classLoader.getResourceAsStream("config.yaml")
    val data: Map<String, Any> = yaml.load(inputStream)
    println(data)
}
```

Skriv YAML:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileWriter

fun main() {
    val yaml = Yaml()
    val data = mapOf("name" to "Erik", "occupation" to "Developer")
    val writer = FileWriter("config.yaml")
    yaml.dump(data, writer)
}
```

Resultatet blir lagrade nyckel-värde-par i `config.yaml`.

## Deep Dive
YAML, "YAML Ain't Markup Language", introducerades i början av 2000-talet. Det var tänkt som ett enklare alternativ till XML. Andra format som JSON är också populära, men YAML används ofta där mänsklig läsbarhet prioriteras. Implementationsdetaljer kan variera beroende på bibliotek, så det är viktigt att referera till dokumentationen för det specifika YAML-bibliotek du använder.

## See Also
- YAML-specifikationen: https://yaml.org/spec/1.2/spec.html
- SnakeYAML-dokumentation: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- Jämförelse mellan JSON och YAML: https://phoenixnap.com/kb/yaml-vs-json-vs-xml
