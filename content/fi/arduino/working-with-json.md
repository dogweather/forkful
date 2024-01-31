---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "JSON-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON-tietueet ovat kevyitä datanvaihtoformaatteja. Ohjelmoijat käyttävät JSONia datan tallentamiseen ja verkon yli siirtämiseen, koska se on helposti luettavaa ja ymmärrettävää ihmisille sekä koneille.

## How to:
Arduino-kirjasto `ArduinoJson` helpottaa JSONin käsittelyä. Asenna kirjasto ensin Library Managerin kautta. Seuraavassa koodissa luodaan ja tulostetaan yksinkertainen JSON-objekti:

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc;

  doc["sensor"] = "gps";
  doc["time"] = 1351824120;

  JsonObject location = doc.createNestedObject("location");
  location["lat"] = 48.756080;
  location["lon"] = 2.302038;

  serializeJson(doc, Serial);
}

void loop() {
  // Ei mitään.
}
```

Tulostus:
```
{"sensor":"gps","time":1351824120,"location":{"lat":48.75608,"lon":2.302038}}
```

## Deep Dive:
JSON (JavaScript Object Notation) syntyi 2000-luvun alussa. Se on noussut XML:n rinnalle, ja monessa tapauksessa syrjäyttänytkin sen, pääasiassa keveytensä ja yksinkertaisuutensa ansiosta. `ArduinoJson` käsittelee JSON-dataa tehokkaasti, mutta on tärkeää valita oikean kokoisen `JsonDocument`:n muistin käytön optimoimiseksi.

## See Also:
- ArduinoJson-kirjaston kotisivu: https://arduinojson.org/
- JSON-standardin spesifikaatio: https://www.json.org/json-fi.html
- Oppaita ja tutoriaaleja JSONin kanssa työskentelystä: https://www.w3schools.com/js/js_json_intro.asp
