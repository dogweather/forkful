---
title:                "Arbeid med YAML"
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et menneskelesbart dataformat for konfigurasjonsfiler og datautveksling. Programmerere bruker YAML fordi det er lett å skrive og lese, og det passer bra for konfigurering av applikasjoner og tjenester.

## Hvordan gjøre det:
For å jobbe med YAML i Kotlin, kan vi bruke et bibliotek som `snakeyaml`. Først, legg til avhengigheten i `build.gradle`:

```kotlin
dependencies {
    implementation("org.yaml:snakeyaml:1.29")
}
```

Deretter, kod et eksempel som leser en enkel YAML-fil:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.File

fun main() {
    val yaml = Yaml()
    val inputFile = File("config.yaml")
    val data = yaml.load<Map<String, Any>>(inputFile.inputStream())

    println(data["key"]) // Ersätt "key" med faktisk nøkkel fra YAML.
}
```

`config.yaml` kan se slik ut:
```yaml
key: Verdi
```

Og output blir:
```
Verdi
```

## Dypdykk:
YAML, som står for "YAML Ain't Markup Language", ble introdusert tidlig på 2000-tallet og er et alternativ til XML og JSON for data serialisering. Det er spesielt populært i DevOps-praksis, med verktøy som Docker og Kubernetes. YAMLs designprinsipper inkluderer leselighet og støtte for datatyper som lister og ordbøker (maps), noe som ikke naturlig støttes av properties-filer. SnakeYAML-biblioteket er en populær Java-implementasjon for å jobbe med YAML, og fungerer fint med Kotlin.

## Se Også:
- [SnakeYAML Engine dokumentasjon](https://bitbucket.org/asomov/snakeyaml-engine)
- [Offisiell YAML nettside](https://yaml.org/)
- [Kotlin dokumentasjon](https://kotlinlang.org/docs/reference/)
- [Eksempel på YAML filer i Kubernetes](https://kubernetes.io/docs/concepts/configuration/overview/)
