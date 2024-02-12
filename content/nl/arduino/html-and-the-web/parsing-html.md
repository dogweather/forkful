---
title:                "HTML Parsen"
aliases:
- /nl/arduino/parsing-html/
date:                  2024-01-28T22:03:26.234218-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
HTML parsen betekent door HTML-code heen zeven om nuttige stukjes te extraheren - zoals het schrapen van een telefoonnummer van een contactpagina. Waarom doen we het? Om gegevensverzameling te automatiseren of om met webpagina's te interageren vanuit je Arduino-project.

## Hoe te:
Arduino is van nature niet web-vaardig, maar met externe modules (zoals ESP8266) kun je verbinden en webinhoud ophalen. Hier halen we HTML op en zoeken we naar een specifieke tag:

```Arduino
#include <ESP8266WiFi.h>
#include <WiFiClient.h>

const char* ssid = "jouwSSID";
const char* password = "jouwWACHTWOORD";

const char* host = "voorbeeld.com";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  WiFiClient client;
  
  if (!client.connect(host, 80)) {
    Serial.println("Verbinding mislukt");
    return;
  }
  
  client.println("GET / HTTP/1.1");
  client.print("Host: ");
  client.println(host);
  client.println("Verbinding: sluiten");
  client.println();

  while(client.connected() || client.available()) {
    if (client.available()) {
      String regel = client.readStringUntil('\n');
      if (regel.indexOf("<title>") >= 0) {
        int startIndex = regel.indexOf("<title>") + 7;
        int endIndex = regel.indexOf("</title>");
        String paginaTitel = regel.substring(startIndex, endIndex);
        Serial.println(paginaTitel);
      }
    }
  }
}

void loop() {
  // We voeren de setup eenmaal uit en verkrijgen de info waarnaar we op zoek zijn. Het is niet nodig om te herhalen.
}
```

Voorbeelduitvoer:
```
Voorbeeld Domein
```

## Diepgaande duik:
Historisch gezien waren microcontrollers zoals Arduino niet ontworpen voor complexe taken zoals HTML-parsing. Dat veranderde allemaal met netwerkcapabele modules en bibliotheken die hun mogelijkheden verrijkten.

De sleutel tot HTML-parsing is stringmanipulatie. Je zoekt naar patronen. Maar onthoud, HTML kan rommelig zijn. Het is niet zoals JSON met zijn betrouwbare structuur. Deze aanpak werkt voor eenvoudige taken, maar kan falen als de HTML onverwacht verandert.

Alternatieven? Zeker. Als je serieus bent over parsen, overweeg dan een met Arduino compatibele microcontroller met meer kracht of die Linux kunnen draaien, wat tools zoals Python opent met bibliotheken die zijn ontworpen voor web scraping.

De eenvoud van Arduino is hier zowel een zegen als een ketting. Je kunt basis-parsing zonder gedoe implementeren, maar als je complexe HTML of enorme hoeveelheden gegevens moet afhandelen, ben je je Uno ontgroeid.

## Zie ook:
- [ESP8266 GitHub-repository](https://github.com/esp8266/Arduino)
- [Arduino HttpClient-bibliotheek](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Web scraping met Python](https://realpython.com/python-web-scraping-practical-introduction/)
