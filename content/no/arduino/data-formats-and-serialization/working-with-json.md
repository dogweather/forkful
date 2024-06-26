---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:44.475412-07:00
description: "Hvordan: For \xE5 jobbe med JSON i Arduino, er `ArduinoJson`-biblioteket\
  \ et popul\xE6rt valg p\xE5 grunn av dets brukervennlighet og effektivitet. Det\
  \ tillater\u2026"
lastmod: '2024-03-13T22:44:41.079546-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 jobbe med JSON i Arduino, er `ArduinoJson`-biblioteket et popul\xE6\
  rt valg p\xE5 grunn av dets brukervennlighet og effektivitet."
title: Arbeider med JSON
weight: 38
---

## Hvordan:
For å jobbe med JSON i Arduino, er `ArduinoJson`-biblioteket et populært valg på grunn av dets brukervennlighet og effektivitet. Det tillater parsing av JSON-strenger, modifisering av dem, og serialisering av objekter tilbake til JSON-strenger. Her er hvordan du bruker det:

1. **Installer ArduinoJson-biblioteket**: Bruk bibliotekbehandleren i Arduino IDE og installer "ArduinoJson".

2. **Deserialisering av en JSON-streng**: Her er hvordan du analyserer en JSON-streng og ekstraherer verdier.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // Juster størrelse i henhold til JSON-dokumentet
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() mislyktes: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float breddegrad = doc["data"][0]; // 48.756080
  float lengdegrad = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(breddegrad, 6);
  Serial.println(lengdegrad, 6);
}

void loop() {
  // Tom løkke
}
```

Eksempel på utdata:

```
gps
1351824120
48.756080
2.302038
```

3. **Serialisering til en JSON-streng**: Her er hvordan du oppretter en JSON-streng fra data.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // Juster størrelse i henhold til data
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // Tom løkke
}
```

Eksempel på utdata (formatert for lesbarhet):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

Bruken av `ArduinoJson`-biblioteket muliggjør effektivt at Arduino-prosjekter kan kommunisere komplekse datastrukturer i et menneskelesbart format, noe som letter utvikling og integrering med webtjenester.
