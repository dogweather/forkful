---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:40.027050-07:00
description: "JSON, oder JavaScript Object Notation, ist ein leichtgewichtiges Daten-Austauschformat,\
  \ was es perfekt f\xFCr die Datenspeicherung oder Konfigurationsdateien\u2026"
lastmod: '2024-03-11T00:14:28.066679-06:00'
model: gpt-4-0125-preview
summary: "JSON, oder JavaScript Object Notation, ist ein leichtgewichtiges Daten-Austauschformat,\
  \ was es perfekt f\xFCr die Datenspeicherung oder Konfigurationsdateien\u2026"
title: Arbeiten mit JSON
---

{{< edit_this_page >}}

## Was & Warum?

JSON, oder JavaScript Object Notation, ist ein leichtgewichtiges Daten-Austauschformat, was es perfekt für die Datenspeicherung oder Konfigurationsdateien in Arduino-Projekten macht. Programmierer verwenden es wegen seiner Einfachheit und Lesbarkeit in verschiedenen Programmierumgebungen, einschließlich Arduino, was den nahtlosen Datenaustausch mit Web-APIs oder anderen Systemen ermöglicht.

## Wie geht das:

Um mit JSON in Arduino zu arbeiten, ist die `ArduinoJson`-Bibliothek eine beliebte Wahl aufgrund ihrer Benutzerfreundlichkeit und Effizienz. Sie ermöglicht das Parsen von JSON-Strings, das Modifizieren dieser und das Serialisieren von Objekten zurück in JSON-Strings. Hier ist, wie man sie verwendet:

1. **Installiere die ArduinoJson-Bibliothek**: Verwende den Bibliotheks-Manager in der Arduino IDE und installiere "ArduinoJson".

2. **Deserialisiere einen JSON-String**: So kannst du einen JSON-String parsen und Werte extrahieren.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // Größe an das JSON-Dokument anpassen
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() fehlgeschlagen: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitude = doc["data"][0]; // 48.756080
  float longitude = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // Leerlauf
}
```

Beispielausgabe:

```
gps
1351824120
48.756080
2.302038
```

3. **Serialisiere zu einem JSON-String**: So erstellst du einen JSON-String aus Daten.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // Größe anhand der Daten anpassen
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // Leerlauf
}
```

Beispielausgabe (formatiert für die Lesbarkeit):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

Die Verwendung der `ArduinoJson`-Bibliothek ermöglicht es Arduino-Projekten effektiv, komplexe Datenstrukturen in einem menschenlesbaren Format zu kommunizieren, was die Entwicklung und Integration mit Webdiensten erleichtert.
