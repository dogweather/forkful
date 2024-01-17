---
title:                "Jobbe med json"
html_title:           "Arduino: Jobbe med json"
simple_title:         "Jobbe med json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-json.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Hvis du jobber med å programmere Arduino, er sjansen stor for at du også kommer til å jobbe med JSON. JSON står for JavaScript Object Notation, og handler om å strukturere data på en enkel og lesbar måte. Dette er viktig fordi det gjør det enklere for programmeringskoden å håndtere og manipulere data.

# Hvordan:

Først må du inkludere JSON-biblioteket i kodeteksten din ved å skrive ```#include <ArduinoJson.h>``` øverst i koden din.

For å kunne jobbe med JSON-data, må du først konvertere dine data til en JSON-syntaks. Dette gjøres ved å bruke funksjonen ```serializeJson()```. La oss se et enkelt eksempel:

```
// Opprett en variabel med data
int sensorData = 50;

// Opprett et tomt JSON-objekt
StaticJsonDocument<20> jsonDocument;

// Legg til data i JSON-objektet
jsonDocument["data"] = sensorData;

// Konverter JSON-objektet til en streng
String jsonString;
serializeJson(jsonDocument, jsonString);

// Skriv ut JSON-strengen til seriell monitor
Serial.println(jsonString);
```

I dette eksempelet har vi opprettet et JSON-objekt med dataen vår og konvertert det til en streng som vi kan skrive ut. Resultatet vil være ```{"data": 50}```.

# Fordypning

JSON har blitt en populær måte å strukturere og lagre data på, spesielt innen webutvikling. Det er et fleksibelt format som støttes av mange ulike programmeringsspråk, inkludert Arduino.

Alternativer til å jobbe med JSON på Arduino inkluderer å bruke et annet dataformat, som for eksempel XML, eller å lage dine egne datastrukturer. Imidlertid kan JSON være enklere å jobbe med og mer standardisert.

Det finnes også andre måter å konvertere data til JSON-syntaks på enn å bruke ```serializeJson()```. For eksempel kan du bruke ```JsonArray``` og ```JsonObject```-klassene for å bygge opp komplekse JSON-datastrukturer.

# Se også

Du kan finne mer informasjon om å jobbe med JSON på Arduino i følgende ressurser:
- [ArduinoJson dokumentasjon](https://arduinojson.org/)
- [Bruk av JSON-biblioteket på Arduino Playground](https://playground.arduino.cc/Code/Json/)
- [Video tutorial om å konvertere data til JSON på Arduino](https://www.youtube.com/watch?v=T_BQX5hH_HU)