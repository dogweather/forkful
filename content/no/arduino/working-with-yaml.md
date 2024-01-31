---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et dataformat brukt til konfigurasjon og datautveksling. Programmerere velger YAML fordi det er lett å lese og skrive, samtidig som det er maskinvennlig.

## Hvordan:
Arduino støtter ikke YAML ut av boksen, men du kan bruke enkel parsing. Her er et eksempel:

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
  
  const char* yaml = 
    "- Grensesnitt: Serial\n"
    "  Hastighet: 9600\n"
    "- Grensesnitt: LCD\n"
    "  Størrelse: 16x2\n";

  DynamicJsonDocument doc(1024);
  deserializeJson(doc, yaml);
  
  JsonArray array = doc.as<JsonArray>();
  for(JsonVariant v : array) {
    Serial.println(v["Grensesnitt"].as<const char*>());
  }
}

void loop() {
  // Kjør kode her
}
```

Sample output:
```
Serial
LCD
```

## Dypdykk
YAML, forkortelse for "YAML Ain't Markup Language" (opprinnelig "Yet Another Markup Language"), ble lansert i 2001. Det konkurrerer med JSON og XML, men er spesielt populært for konfigurasjonsfiler. Direkte støtte for YAML i Arduino finnes ikke, så en må parse som JSON eller annet format, eller bruke eksterne biblioteker.

## Se Også
- YAML offisiell side: [YAML](https://yaml.org)
- ArduinoJson bibliotek Docs: [ArduinoJson](https://arduinojson.org/)
- YAML til JSON konverteringsverktøy: [YAML to JSON](https://www.json2yaml.com/)
