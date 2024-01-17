---
title:                "Arbeid med yaml"
html_title:           "C: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML står for "YAML Ain't Markup Language" og er et format for å lagre og strukturere data. Det brukes ofte av programmerere til å konfigurere og lagre data på en lesbar og organisert måte.

## Hvordan:
For å jobbe med YAML i C programmeringsspråk, må du inkludere biblioteket "yaml.h". Her er et eksempel på hvordan du kan skrive ut en enkel YAML-fil:

```
#include <yaml.h>

void main() {
    YAML::Emitter emitter;
    emitter << YAML::BeginMap        // starter en "map"
            << YAML::Key << "navn"   // skriver ut "navn:"
            << YAML::Value << "Peter"// skriver ut "Peter"
            << YAML::EndMap;          // avslutter "map"
    std::cout << emitter.c_str();     // skriver ut YAML-filen
}

```
Output:
```
navn: Peter
```
## Dypdykk:
YAML ble opprinnelig utviklet i 2001 av Ingy döt Net, men ble standardisert i 2005 av YAML.org. Det er en enkel og fleksibel måte å lagre og transportere data mellom forskjellige programmer og programmeringsspråk.

En alternativ måte å konfigurere og lagre data på er ved bruk av XML, men YAML er ofte foretrukket fordi det er mer leselig og mindre "klumpete". 

Implementeringen av YAML i C er mulig ved hjelp av biblioteket "yaml.h", som er åpen kildekode og lett å integrere i dine eksisterende prosjekter.

## Se også:
- [YAML.org](https://yaml.org/) for offisiell dokumentasjon og nyheter om YAML
- [C YAML](https://github.com/yaml/libyaml) for mer informasjon om å jobbe med YAML i C
- [YAML vs. XML](https://yaml.org/docs/history.html) for en sammenligning mellom YAML og XML