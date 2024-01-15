---
title:                "Å jobbe med json"
html_title:           "Arduino: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du er en nybegynner eller en erfaren Arduino-bruker, er det alltid spennende å lære nye programmeringskonsepter. JSON er et effektivt og populært format for å utveksle data mellom enheter og plattformer. Lære å jobbe med JSON på Arduino kan bidra til å utvide dine programmeringsferdigheter og åpne døren for nye prosjekter.

## Slik gjør du det

For å jobbe med JSON på Arduino, må du følge disse enkle trinnene:

1. Installer JSON biblioteket: Åpne Arduino IDE og gå til \"Verktøy\" og velg \"Inkluder bibliotek\" og deretter \"Administrer bibliotek\". Søk etter \"ArduinoJson\" biblioteket og klikk på \"Installer\".

2. Importer biblioteket: Legg til følgende kode på toppen av ditt Arduino skisse:

```Arduino
#include <ArduinoJson.h>
```

3. Opprett et JSON objekt: Lag et tomt JSON objekt ved å bruke følgende kode:

```Arduino
StaticJsonDocument<200> jsonBuffer; //Buffer på 200 bytes
```

4. Kodingseksempel: Her er et eksempel på hvordan du kan lagre og lese data fra et JSON objekt:

```Arduino
// Lagre data
jsonBuffer["sensor"] = "temperatur";
jsonBuffer["verdi"] = 25.5;

// Skriv ut data
Serial.println(jsonBuffer["sensor"]); // Output: "temperatur"
Serial.println(jsonBuffer["verdi"]); // Output: 25.5
```

5. Parsing: For å hente data fra et JSON objekt, kan du bruke metoden `parseObject()` med å sende inn din JSON streng som en parameter:

```Arduino
// Et JSON eksempel streng
char json[] = "{\"farge\": \"blå\", \"størrelse\": \"medium\"}";

// Parse og skriv ut data
JsonObject& root = jsonBuffer.parseObject(json);
Serial.println(root["farge"]); // Output: "blå"
Serial.println(root["størrelse"]); // Output: "medium"
```

## Dykk dypere

JSON har en fleksibel og hierarkisk struktur, så du kan bruke indeksering for å jobbe med data som kan arrangeres som et kart eller et tre. For å få mer detaljert informasjon om hvordan du jobber med JSON på Arduino, kan du besøke bibliotekets dokumentasjon.

## Se også

- ArduinoJson bibliotekets offisielle nettsted: https://arduinojson.org/
- Dokumentasjon og eksempler: https://arduinojson.org/v6/api/