---
title:                "Arduino: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en Arduino-entusiast eller en hobbyist som er interessert i å lage dine egne elektroniske prosjekter, er sjansen stor for at du allerede er kjent med konseptet JSON. JSON, eller JavaScript Object Notation, er et lettvektsformat for datautveksling som er mye brukt i moderne programmering. Å lære å arbeide med JSON kan være nyttig for å integrere data fra ulike kilder i dine Arduino-prosjekter.

## Hvordan

For å begynne å arbeide med JSON på Arduino, må du først sørge for at du har den nyeste versjonen av Arduino IDE installert på datamaskinen din. Deretter kan du følge disse trinnene:

1. Opprett et nytt Arduino-prosjekt og gi filen navnet "json_example.ino".
2. Legg til følgende linjer øverst i koden din for å inkludere biblioteket "ArduinoJson":

```Arduino
#include <ArduinoJson.h>
```

3. Definer en variabel for å lagre JSON-dataen din, for eksempel:

```Arduino
String json = "{\"id\": 1234, \"navn\": \"Arduino\", \"versjon\": \"1.0\"}"; 
```

4. Bruk "StaticJsonDocument" -funksjonen for å analysere og lagre JSON-dataen din i et dokument:

```Arduino
StaticJsonDocument<200> doc; 
DeserializationError error = deserializeJson(doc, json); 
```

5. Du kan nå få tilgang til dine ulike data ved hjelp av "doc" -variabelen. For eksempel, for å skrive ut "navn" -dataen, kan du bruke:

```Arduino
Serial.println(doc["navn"].as<char*>()); // Utskrift: "Arduino"
```

## Dypdykk

Å arbeide med JSON på Arduino kan være utfordrende, spesielt hvis du jobber med store mengder data. Det er viktig å sørge for at du definerer riktig størrelse på "StaticJsonDocument" for å unngå minnefeil. Du bør også være klar over forskjellen mellom "data" og "minne" når du arbeider med JSON, da dette kan påvirke utførelsen av programmet ditt. Å lese dokumentasjonen til ArduinoJson-biblioteket kan også være nyttig for å forstå alle funksjoner og muligheter som tilbys.

## Se også

- [ArduinoJson dokumentasjon](https://arduinojson.org/)
- [Eksempelkodene for ArduinoJson](https://github.com/bblanchon/ArduinoJson/tree/master/examples) for å se flere måter å arbeide med JSON på Arduino.