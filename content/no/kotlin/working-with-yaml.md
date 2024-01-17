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

# Hva & Hvorfor?
YAML står for "YAML Ain't Markup Language" og er en enkel måte for programmerere å lagre og dele strukturert data. Det er mye brukt for konfigurasjonsfiler, da det er enkelt å lese og skrive for både mennesker og maskiner.

# Hvordan:
Kotlin har innebygd støtte for YAML ved hjelp av biblioteket SnakeYAML. For å bruke dette biblioteket, må du først importere det i prosjektet ditt:

```Kotlin
implementation 'org.yaml:snakeyaml:1.27'
```

Deretter kan du enkelt lese og skrive YAML-filer ved hjelp av følgende kode:

```Kotlin
// Les en YAML-fil
val input = FileInputStream("min_fil.yaml")
val yaml = Yaml()
val data = yaml.load(input)

// Skriv til en YAML-fil
val output = FileWriter("min_ut_fil.yaml")
yaml.dump(data, output)
```

# Dypdykk:
YAML ble utviklet i 2001 som en enklere alternativ til XML og JSON for å representere datastrukturer. Det brukes i stor grad i konfigurasjonsfiler for programmer og har også fått støtte i populære programmeringsspråk som Java, Python og selvfølgelig Kotlin.

Andre alternativer til YAML inkluderer XML og JSON, men YAML skiller seg ut på grunn av sin enkle og menneskelesbare syntaks. En ulempe med YAML er at det ikke er like strukturert som XML, noe som kan føre til problemer med validering.

Den grunnleggende syntaksen til YAML er basert på innrykk og bruk av ulike spesialtegn som kolon og streker. Det er viktig å være nøye med innrykk og bruke riktige spesialtegn for å unngå feil i YAML-filen.

# Se Også:
- [SnakeYAML dokumentasjon] (https://bitbucket.org/asomov/snakeyaml/src/default/)
- [Introduksjon til YAML] (https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html)
- [JSON vs XML vs YAML] (https://stackoverflow.com/questions/3536898/json-vs-xml-vs-yaml)