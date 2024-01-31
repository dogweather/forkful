---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?

YAML ist ein Format zur Datendarstellung, das oft in Konfigurationsdateien verwendet wird, weil es gut lesbar ist. Programmierer nutzen es, um menschenlesbare Daten zu speichern und zu teilen, oft in Projekten wie der IoT-Kommunikation oder der Konfiguration von Software.

## Anleitung:

```Arduino
// Hinweis: Für YAML auf Arduino benötigst du eine Bibliothek wie "ArduinoJson".
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Beispielhafter YAML-String:
  const char* yaml = 
    "- Zeit: 2023-10-06 08:15\n"
    "  Nachricht: Temperaturmessung\n"
    "  Wert: 23.7\n"
    "- Zeit: 2023-10-06 08:16\n"
    "  Nachricht: Luftfeuchtigkeit\n"
    "  Wert: 40";

  DynamicJsonDocument doc(1024);
  DeserializationError error = deserializeJson(doc, yaml);

  if (error) {
    Serial.print("deserializeJson() fehlgeschlagen: ");
    Serial.println(error.c_str());
    return;
  }

  JsonArray array = doc.as<JsonArray>();

  for (JsonObject obj : array) {
    Serial.println(obj["Zeit"].as<String>());
    Serial.println(obj["Nachricht"].as<String>());
    Serial.println(obj["Wert"].as<float>());
  }
}

void loop() {
  // Leere loop, da Beispiel nur bei Initialisierung läuft.
}
```

Sample Output:

```
2023-10-06 08:15
Temperaturmessung
23.7
2023-10-06 08:16
Luftfeuchtigkeit
40
```

## Deep Dive:

YAML kommt von "YAML Ain’t Markup Language" (ursprünglich "Yet Another Markup Language") und existiert seit 2001. Es ist eine klare Alternative zu XML und JSON für Konfigurationsdateien und wird wegen seiner Übersichtlichkeit geschätzt. YAML-Implementationen in Arduino-Projekten nutzen oft externe Bibliotheken, wie ArduinoJson, weil die Sprachunterstützung nativ begrenzt ist.

## See Also:

- YAML Spezifikation: https://yaml.org/spec/1.2/spec.html
- ArduinoJson Bibliothek: https://arduinojson.org/
- YAML-Tutorials für tiefgreifendere Unterweisungen: https://learnxinyminutes.com/docs/yaml/
