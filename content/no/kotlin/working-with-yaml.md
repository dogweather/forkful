---
title:                "Å jobbe med yaml"
html_title:           "Kotlin: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med YAML kan være en nyttig ferdighet å lære for å jobbe med konfigurasjonsfiler, spesielt i web- og mobilapplikasjoner. Det er et enkelt og leselig format som gjør det enkelt å organisere og strukturere data.

## Hvordan

For å begynne å jobbe med YAML i Kotlin, må du først importere "kaml" biblioteket i ditt prosjekt. Deretter kan du begynne å lage og behandle YAML-filer.

```Kotlin
// Importere Yaml-klassen fra kaml-biblioteket
import kaml.Yaml

// Opprette en YAML-fil
val yaml = """
    navn: John Smith
    alder: 25
    favorittfarge: blå
""".trimIndent()

// Endre en verdi og lagre filen
yaml.put("favorittfarge", "rød")
```

#### Resultat:

```Kotlin
navn: John Smith
alder: 25
favorittfarge: rød
```

## Dypdykk

I tillegg til å lage og endre YAML-filer, kan du også bruke kaml biblioteket til å konvertere YAML til Kotlin dataklasser og vice versa. Dette er nyttig når du arbeider med større og mer komplekse datastrukturer.

```Kotlin
// Opprette en dataklasse
data class Person(val navn: String, val alder: Int, val favorittfarge: String)

// Konvertere YAML til Kotlin dataklasse
val person = Yaml.default.decodeFromString(Person.serializer(), yaml)

// Endre favorittfargen
person.favorittfarge = "grønn"

// Konvertere tilbake til YAML-fil
val nyttYaml = Yaml.default.encodeToString(Person.serializer(), person)
```

#### Resultat:

```Kotlin
navn: John Smith
alder: 25
favorittfarge: grønn
```

## Se Også

- [Offisiell Kotlin Dokumentasjon om YAML](https://kotlinlang.org/docs/yaml.html)
- [Kaml Biblioteket Dokumentasjon](https://github.com/MicroUtils/kotlin-logging#configuration)
- [En Gjennomgang av YAML Syntaks](https://www.baeldung.com/yaml-syntax-kotlin)