---
title:                "Arbeta med json"
html_title:           "Arduino: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
Arduino är ett populärt och enkelt sätt att skapa interaktiva projekt med hårdvara. Genom att lära sig hur man arbetar med JSON på Arduino kan du utöka dess funktionalitet och integrera det med andra enheter och webbtjänster.

## Så här gör du
Att arbeta med JSON på Arduino involverar att använda en tredjepartsbibliotek som heter "ArduinoJson". Här är en snabb guide för att komma igång:

1. Installera ArduinoJson biblioteket genom att gå till *Verktyg -> Hantera Bibliotek* i din Arduino-IDE.
2. Sök efter "ArduinoJson" och välj den senaste versionen för installation.
3. Skapa ett nytt Arduino-sketch och inkludera "ArduinoJson.h" i början av koden.
4. Definiera en "ArduinoJson" objekt och välj storleken på bufferten.
5. Skapa ett JSON-dokument i en strängvariabel och använd funktionen `.printTo()` för att skriva ut det till bufferten.
6. Använd `deserializeJson()` funktionen för att läsa in dokumentet från bufferten och tilldela värden till variabler.
7. Använd dina variabler i Arduino-koden för att skapa interaktiva projekt.

```Arduino
#include <ArduinoJson.h>

StaticJsonDocument<200> doc;

String jsonString = "{\"sensor\": \"temperature\",\"value\": 25.5}";

// Skriva ut JSON till bufferten
doc.clear();
deserializeJson(doc, jsonString);

String sensor = doc["sensor"];
float value = doc["value"];

Serial.println(sensor); // output: temperature
Serial.println(value); // output: 25.5
```

## Deep Dive
JSON (JavaScript Object Notation) är ett lättläst format för datautbyte som har blivit väldigt populär i webbutveckling och Internet of Things (IoT). Det är baserat på JavaScripts objekt och kan enkelt läsas och skrivas av människor och maskiner. JSON-filen består av olika objekt och värden som är strukturerade i en hierarki med hjälp av nycklar och värden.

En stor fördel med att använda JSON på Arduino är dess lätthet och effektivitet. ArduinoJson biblioteket är utformat för att hantera data på ett effektivt sätt och gör det möjligt att både läsa och skriva JSON-data på en bråkdel av tiden som det skulle ta att göra det manuellt.

Det finns också flera olika metoder i ArduinoJson som gör det möjligt att manipulera och åtkomst JSON-data på olika sätt, vilket ger stor flexibilitet och funktionalitet. Det är ett kraftfullt verktyg som kan hjälpa dig att skapa mer komplexa och integrerade projekt med Arduino.

## Se även
- [ArduinoJson dokumentation](https://arduinojson.org/doc/)
- [Introduktion till JSON på Arduino](https://www.arduino.cc/en/Reference/ArduinoJson)
- [Introduktion till JSON](https://www.json.org/json-sv.html)