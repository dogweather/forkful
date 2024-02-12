---
title:                "Werken met YAML"
aliases:
- /nl/arduino/working-with-yaml/
date:                  2024-01-28T22:11:24.039567-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
YAML is geen opmaaktaal. Het is een door mensen gemakkelijk te begrijpen gegevensserialisatiestandaard voor alle programmeertalen. Programmeurs gebruiken het voor configuratiebestanden, gegevensuitwisseling tussen talen, en het is makkelijker te begrijpen vergeleken met XML of JSON.

## Hoe te:
Arduino kan standaard niet met YAML overweg. Om het te gebruiken, gebruik je een externe bibliotheek. Bijvoorbeeld:

Installeer de bibliotheek "ArduinoJson" via Bibliothekenbeheer. Gebruik `DynamicJsonDocument` voor parsing:

```Arduino
#include <ArduinoJson.h>

const char* yaml = 
  "- titel: De vanger in het graan\n"
  "  auteur: J.D. Salinger\n"
  "- titel: Negenentachtig\n"
  "  auteur: George Orwell\n";

void setup() {
  Serial.begin(9600);
  DynamicJsonDocument doc(1024);
  deserializeJson(doc, yaml);
  for (JsonObject elem : doc.as<JsonArray>()) {
    Serial.println(elem["titel"].as<String>());
    Serial.println(elem["auteur"].as<String>());
  }
}

void loop() {
  // niet gebruikt in dit voorbeeld
}
```

Voorbeelduitvoer:

```
De vanger in het graan
J.D. Salinger
Negenentachtig
George Orwell
```

## Diepgaand
YAML is ontstaan in de vroege jaren 2000, opgebouwd voor menselijke leesbaarheid. Als een JSON-superset, is elk JSON-bestand ook een geldig YAML. Veelvoorkomende alternatieven zijn JSON of XML, maar de minimale syntaxis van YAML streeft naar beter beheer door mensen zonder extra opsmuk. YAML op Arduino parsen betekent YAML converteren naar JSON met behulp van externe tools en vervolgens de JSON gebruiken in je schetsen.

## Zie Ook
- Officiële YAML website: https://yaml.org
- ArduinoJson GitHub-repository: https://github.com/bblanchon/ArduinoJson
- YAML naar JSON online converter: https://www.json2yaml.com/
