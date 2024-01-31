---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
- YAML on data-muoto, joka on helppolukuinen ja johon on helppo kirjoittaa.
- Koodarit käyttävät YAML:ää konfiguraatiotiedostoihin ja datan tallentamiseen, koska se on selkeä ja ymmärrettävä.

## How to:
```arduino
#include <ArduinoJson.h>

// An example of YAML data as a string
const char* yaml = 
"date: 2023-02-28\n"
"time: 19:30:00\n"
"event: Arduino Workshop\n";

void setup() {
  StaticJsonDocument<200> doc;
  deserializeJson(doc, yaml);
  Serial.begin(9600);
  while (!Serial) {
    // wait for serial port to connect
  }
  
  int day = doc["date"].as<String>().substring(8, 10).toInt();
  int month = doc["date"].as<String>().substring(5, 7).toInt();
  int year = doc["date"].as<String>().substring(0, 4).toInt();
  String event = doc["event"].as<String>();
  
  Serial.print("Event: ");
  Serial.println(event);
  Serial.print("Date: ");
  Serial.print(day);
  Serial.print(".");
  Serial.print(month);
  Serial.print(".");
  Serial.println(year);
}

void loop() {
  // nothing to do here
}
```

## Deep Dive
- YAML syntyi 2000-luvun alussa, korvaamaan tai täydentämään XML:ää.
- Vaihtoehtoina ovat JSON ja XML.
- YAML käsittelee listoja, sanakirjoja ja skalaareja. C++:ssa YAMLin käsittely vaatii kirjastoa, kuten ArduinoJson.

## See Also
- YAML virallinen sivusto: [https://yaml.org/](https://yaml.org/)
- ArduinoJson kirjaston GitHub-sivu: [https://github.com/bblanchon/ArduinoJson](https://github.com/bblanchon/ArduinoJson)
- YAML Wikipedia: [https://fi.wikipedia.org/wiki/YAML](https://fi.wikipedia.org/wiki/YAML)
