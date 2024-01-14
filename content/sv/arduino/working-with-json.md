---
title:                "Arduino: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Varför arbeta med JSON i Arduino

Att använda JSON i Arduino öppnar upp en värld av möjligheter för att kommunicera med andra enheter och webbtjänster. Genom att använda JSON kan du effektivt skicka och ta emot data, vilket är viktigt för många Internet of Things (IoT) projekt.

## Hur man använder JSON i Arduino

Det första steget är att installera en JSON-läsningsbibliotek som till exempel ArduinoJson. Sedan kan du enkelt konvertera en sträng till JSON-objekt med följande kod:

```Arduino
#include <ArduinoJson.h> // inkluderar biblioteket
int led = 13; // ange en pin till LED-lampa
String jsonString = "{\"sensor\":\"temp\", \"value\":25}"; // strängen som ska konverteras
// konverterar strängen till ett JSON-objekt
DynamicJsonDocument jsonDoc(1024); // storleken på dokumentet måste anges
deserializeJson(jsonDoc, jsonString);
// hämtar värden från JSON-objektet
const char* sensor = jsonDoc["sensor"];
int value = jsonDoc["value"];
```

Du kan också konvertera en JSON-objekt till en sträng för att skicka data till en webbtjänst:

```Arduino
// konverterar ett JSON-objekt till en sträng
StaticJsonDocument<200> jsonDoc; // storleken på dokumentet måste anges
jsonDoc["sensor"] = "light";
jsonDoc["value"] = 50;
String jsonString;
serializeJson(jsonDoc, jsonString);
```

## Djupdykning i JSON i Arduino

Att arbeta med JSON i Arduino kan vara utmanande eftersom Arduinos begränsade minneskapacitet. Därför är det viktigt att optimera din kod för att spara minne. Det finns också andra saker att ta hänsyn till, som till exempel att undvika att skicka för stora JSON-objekt som kan göra att Arduinon kraschar.

## Se även

1. [ArduinoJson biblioteket](https://arduinojson.org/)
2. [Enkel guide för att använda JSON i Arduino](https://create.arduino.cc/projecthub/muhammad-aqib/using-json-in-arduino-cc5101)
3. [Tips för att optimera kod för Arduinos minneskapacitet](https://www.hackster.io/Salmanfarisvp/tips-tricks-for-optimizing-arduino-code-size-8d9b21)