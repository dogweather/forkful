---
title:                "Kotlin: Å arbeide med yaml"
simple_title:         "Å arbeide med yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hvorfor

YAML er et svært nyttig format for å lagre og håndtere data i programmering. Det er enkelt å lese og skrive, og kan brukes til å representere komplekse data på en lesbar måte. Hvis du ønsker å arbeide med data i dine Kotlin-prosjekter, er det vel verdt å lære å bruke YAML.

# Slik gjør du det

For å arbeide med YAML i Kotlin, kan du bruke et bibliotek som Kalidasa. Først må du legge til avhengigheten i din byggefil:

```Kotlin
dependencies {
    implementation("com.eclipsesource.kalidasa:kalidasa:3.0.0")
}
```

For å lese en YAML-fil og konvertere den til en mappe i Kotlin, kan du bruke følgende kode:

```Kotlin
val yamlString = """
    navn: John Doe
    alder: 30
    favorittfarge: rød
""".trimIndent()

val mapper = YamlParser().load(yamlString)
```

Du kan deretter få tilgang til verdiene i mappen som vanlige Kotlin-variabler:

```Kotlin
val navn = mapper["navn"] as String
val alder = mapper["alder"] as Int
val favorittfarge = mapper["favorittfarge"] as String
```

Du kan også opprette en YAML-fil fra en eksisterende mappe i Kotlin ved å bruke følgende kode:

```Kotlin
val mappe = mapOf(
    "navn" to "Jane Doe",
    "alder" to 25,
    "favorittfarge" to "blå"
)

val yamlString = YamlProducer().dump(mappe)
```

# Dypdykk

Når du arbeider med YAML i Kotlin, må du være klar over noen viktige konsepter. Først og fremst er det viktig å skille mellom en YAML-tekststreng og en mappe i Kotlin. En YAML-tekststreng er bare strengrepresentasjonen av et YAML-dokument, mens en mappe i Kotlin er en strukturert samling av data.

Det kan også være lurt å lære om hvordan du konfigurerer Kalidasa-biblioteket for å behandle spesielle YAML-funksjoner, for eksempel tilpassede konverteringer og valideringer.

# Se også

- [Kotlin hjemmeside](https://kotlinlang.org/)
- [YAML spesifikasjon](https://yaml.org/)
- [Kalidasa dokumentasjon](https://github.com/kalidasa-kt/kalidasa/blob/master/docs/overview.md)