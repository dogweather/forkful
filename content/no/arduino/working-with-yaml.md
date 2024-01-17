---
title:                "Å arbeide med yaml"
html_title:           "Arduino: Å arbeide med yaml"
simple_title:         "Å arbeide med yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med YAML i Arduino-programmering handler om å organisere og strukturere data på en effektiv måte. Dette hjelper programmerere med å håndtere store datasett og gjøre endringer enklere på en senere tid. 

## Hvordan:

Arduino har en innebygd bibliotek som gjør det enkelt å jobbe med YAML-data. Følg disse tre trinnene for å komme i gang:

1. Importer YAML-biblioteket ved å legge til følgende linje i begynnelsen av koden din:
```
#include <Arduino_YAML.h>
```
2. Definer en variabel for å lagre og behandle YAML-data:
```
YAMLDocument data;
```
3. Bruk `data` variabelen til å lese, skrive og manipulere YAML-data som vist i følgende eksempel:
```
Arduino_YAML.load("data.yaml", data);
Serial.println(data["navn"]);
```

## Dykk Dypere:

Historisk sett har YAML vært brukt i webutvikling for å strukturere data i et leselig og enkelt format. Alternativer til YAML inkluderer JSON og XML, men YAML anses ofte som mer intuitivt og leselig. I Arduino er YAML spesielt nyttig for deling av data mellom forskjellige enheter og for å holde koden ryddig og organisert.

## Se også:

For mer informasjon om Arduino og YAML, sjekk ut følgende ressurser: 
- Offisiell dokumentasjon for YAML-biblioteket: https://github.com/arduino-libraries/Arduino_YAML
- Arduino forum tråd om YAML: https://forum.arduino.cc/index.php?topic=297175.0