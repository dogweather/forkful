---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON (JavaScript Object Notation) är ett lättviktigt dataformat för att lagra och utbyta data. Programmerare använder JSON för att enkelt överföra data mellan servrar och webbklienter eller mellan olika komponenter i en IoT-lösning där Arduino kan ingå.

## Hur gör man:
Använd biblioteket `ArduinoJson` för att hantera JSON. Låt oss sätta igång med att installera `ArduinoJson` via Bibliotekshanteraren.

Lägg till följande kod för att deserialisera en JSON-sträng:
```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

  DynamicJsonDocument doc(1024);
  deserializeJson(doc, json);

  const char* sensor = doc["sensor"];
  long time = doc["time"];
  double latitude = doc["data"][0];
  double longitude = doc["data"][1];

  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // Gör ingenting här
}
```
Starta din Arduino och öppna Serial Monitor. Förväntad utskrift:
```
gps
1351824120
48.756080
2.302038
```

Nu för att serialisera och skicka JSON:
```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  doc["data"][0] = 48.756080;
  doc["data"][1] = 2.302038;

  serializeJson(doc, Serial);
}

void loop() {
  // Gör ingenting här
}
```
Utskriften är en sträng av JSON-data:
```
{"sensor":"gps","time":1351824120,"data":[48.75608,2.302038]}
```

## Djupdykning
JSON uppstod ur JavaScript-men språkobundenheten är nu helt fristående. Det passar perfekt för inbäddade system som Arduino på grund av dess enkelhet och lättillgänglighet. Det finns alternativ som XML, men JSON är smidigare och använder mindre data vilket är kritiskt för mikrokontroller. `ArduinoJson` är en av de mest populära biblioteken för Arduino och är avgörande för effektiv datahantering.

## Se också
För ytterligare läsning och avancerade exempel, besök:

- Den officiella ArduinoJson-biblioteksdokumentationen: https://arduinojson.org/
- Arduino-guider om hantering av webbklienter och servrar: https://www.arduino.cc/en/Guide
- JSON-specifikationen: https://www.json.org/json-en.html
- Arduino Forum för frågor och community support: https://forum.arduino.cc/