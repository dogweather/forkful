---
title:                "Werken met YAML"
date:                  2024-01-28T22:12:10.054500-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML, wat staat voor "YAML Ain't Markup Language", is een standaard voor het serialiseren van gegevens die leesbaar is voor mensen. Programmeurs gebruiken het om software te configureren, gegevens te definiëren of parameters in te stellen vanwege de veelzijdigheid en leesbaarheid.

## Hoe:

Om met YAML in Kotlin te werken, gebruik je typisch een bibliotheek zoals `snakeyaml`. Laten we duiken in hoe je een YAML-bestand kunt parseren:

Voeg eerst de afhankelijkheid toe in je `build.gradle` bestand:

```kotlin
implementation("org.yaml:snakeyaml:1.29")
```

Laten we nu een eenvoudig YAML-bestand parsen met SnakeYAML:

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

// Voorbeeldinhoud van config.yaml:
// name: John Doe
// age: 30

// Voorbeeldoutput:
// John Doe
// 30
```

Dit codefragment laadt een YAML-bestand en drukt de waarden af die geassocieerd zijn met sleutels `name` en `age`.

## Diepgaande Duik

YAML kwam in de vroege jaren 2000 naar voren om de complexiteit van XML te bestrijden. Het biedt een eenvoudigere syntaxis, waardoor het de voorkeur heeft voor configuratiebestanden. Alternatieven zijn onder andere JSON, dat meer op data is gericht en minder mensvriendelijk is, en TOML, dat enigszins een middenweg is. Bij het omgaan met YAML in Kotlin, bieden bibliotheken zoals `snakeyaml` de parseringsengine, die zich aan je Kotlin-code haakt om YAML-strings om te zetten in native datastructuren.

## Zie Ook

- YAML 1.2 Specificatie: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Kotlin Documentatie: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
