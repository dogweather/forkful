---
title:                "Arbeid med JSON"
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Jobbing med JSON (JavaScript Object Notation) innebærer å bruke et lettvekt og formatert tekstformat for datautveksling. Programmerere benytter JSON fordi det er enkelt å lese for mennesker, det er lett å parse for maskiner, og det støttes bredt på tvers av programmeringsspråk.

## How to:
```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Anta at vi mottar en JSON streng som inneholder temperaturdata
  const char* json = "{\"temperature\": 23.5}";

  // Opprett en JSONBuffer for å holde dataen
  StaticJsonDocument<200> doc;

  // Parse JSON strengen
  deserializeJson(doc, json);

  // Hent verdien (antar at det er en flyttall)
  float temperature = doc["temperature"];

  // Skriv ut til seriell monitor
  Serial.print("Temperatur: ");
  Serial.println(temperature);
}

void loop() {
  // Ingenting å gjøre her
}
```
Resultat:
```
Temperatur: 23.5
```

## Deep Dive
JSON ble introdusert i 2001 og har siden da blitt et av de mest populære formatene for dataoverføring over nettet, mye grunnet sin enkelhet og lesbarhet sammenlignet med XML. I Arduino kan vi håndtere JSON ved hjelp av bibliotek som `ArduinoJson`, som er effektivt og tilgjengelig via Arduino IDE's bibliotekshåndterer. Implemetasjon av JSON i Arduino krever en forståelse av parsing og serialisering for å konvertere mellom tekst og nyttige datastrukturer.

## See Also
- ArduinoJson bibliotekets offisielle side: https://arduinojson.org/
- JSON standardspesifikasjon: https://www.json.org/json-en.html
- Arduino sin referanseguide for Serial: https://www.arduino.cc/reference/en/language/functions/communication/serial/