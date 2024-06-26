---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:34.408401-07:00
description: "Come fare: Per lavorare con JSON in Arduino, la libreria `ArduinoJson`\
  \ \xE8 una scelta popolare grazie alla sua facilit\xE0 d'uso e efficienza. Permette\
  \ di\u2026"
lastmod: '2024-03-13T22:44:43.705622-06:00'
model: gpt-4-0125-preview
summary: "Per lavorare con JSON in Arduino, la libreria `ArduinoJson` \xE8 una scelta\
  \ popolare grazie alla sua facilit\xE0 d'uso e efficienza."
title: Lavorare con JSON
weight: 38
---

## Come fare:
Per lavorare con JSON in Arduino, la libreria `ArduinoJson` è una scelta popolare grazie alla sua facilità d'uso e efficienza. Permette di analizzare le stringhe JSON, modificarle e serializzare gli oggetti di nuovo in stringhe JSON. Ecco come usarla:

1. **Installa la libreria ArduinoJson**: Usa il Gestore delle Librerie nell'IDE di Arduino e installa "ArduinoJson".

2. **Deserializza una stringa JSON**: Ecco come analizzare una stringa JSON ed estrarre i valori.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // Regola la dimensione secondo il documento JSON
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() non riuscito: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensore = doc["sensor"]; // "gps"
  long tempo = doc["time"]; // 1351824120
  float latitudine = doc["data"][0]; // 48.756080
  float longitudine = doc["data"][1]; // 2.302038
  
  Serial.println(sensore);
  Serial.println(tempo);
  Serial.println(latitudine, 6);
  Serial.println(longitudine, 6);
}

void loop() {
  // Ciclo vuoto
}
```

Output di esempio:

```
gps
1351824120
48.756080
2.302038
```

3. **Serializza in una stringa JSON**: Ecco come creare una stringa JSON dai dati.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // Regola la dimensione secondo i dati
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // Ciclo vuoto
}
```

Output di esempio (formattato per leggibilità):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

Utilizzare efficacemente la libreria `ArduinoJson` consente ai progetti Arduino di comunicare strutture dati complesse in un formato facilmente leggibile, facilitando lo sviluppo e l'integrazione con i servizi web.
