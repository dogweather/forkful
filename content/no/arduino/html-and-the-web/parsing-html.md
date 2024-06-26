---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:38.987176-07:00
description: "Hvordan: Parsing av HTML p\xE5 Arduino krever vanligvis biblioteker\
  \ med minimalt fotavtrykk p\xE5 grunn av begrensede enhetsressurser. Et popul\xE6\
  rt valg for\u2026"
lastmod: '2024-03-13T22:44:41.056461-06:00'
model: gpt-4-0125-preview
summary: "Parsing av HTML p\xE5 Arduino krever vanligvis biblioteker med minimalt\
  \ fotavtrykk p\xE5 grunn av begrensede enhetsressurser."
title: Analysering av HTML
weight: 43
---

## Hvordan:
Parsing av HTML på Arduino krever vanligvis biblioteker med minimalt fotavtrykk på grunn av begrensede enhetsressurser. Et populært valg for webskraping og parsing er å bruke `ESP8266HTTPClient`- og `ESP8266WiFi`-bibliotekene for ESP8266, eller tilsvarende biblioteker for ESP32, gitt deres innebygde støtte for Wi-Fi-egenskaper og HTTP-protokoller. Her er et grunnleggende eksempel for å hente og parse HTML, under forutsetning av at du arbeider med en ESP8266 eller ESP32:

Først, inkluder de nødvendige bibliotekene:
```cpp
#include <ESP8266WiFi.h> // For ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Bruk tilsvarende ESP32-biblioteker hvis du bruker en ESP32

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

Koble til Wi-Fi-nettverket ditt:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Kobler til...");
    }
}
```

Gjør en HTTP-forespørsel og parse et enkelt stykke HTML:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Sjekk WiFi-tilkoblingsstatus
        HTTPClient http;  //Deklarer et objekt av klassen HTTPClient

        http.begin("http://example.com");  //Spesifisere forespørselsdestinasjon
        int httpCode = http.GET();  //Send forespørselen

        if (httpCode > 0) { //Sjekk returkoden
            String payload = http.getString();   //Hent forespørselresponsens nyttelast
            Serial.println(payload);             //Skriv ut responsens nyttelast

            // Parse en bestemt del, f.eks. ekstrahere tittel fra nyttelast
            int titleStart = payload.indexOf("<title>") + 7; // +7 for å flytte forbi "<title>"-taggen
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Sidetittel: ");
            Serial.println(pageTitle);
        }

        http.end();   //Lukk tilkoblingen
    }

    delay(10000); //Gjør en forespørsel hvert 10. sekund
}
```

Eksempel på utdata (med utgangspunkt i at http://example.com har en enkel HTML-struktur):
```
Kobler til...
...
Sidetittel: Example Domain
```

Dette eksemplet demonstrerer hvordan man henter en HTML-side og ekstraherer innholdet i `<title>`-taggen. For mer kompleks HTML-parsing kan det vurderes å bruke regulære uttrykk (med forsiktighet på grunn av minnebegrensninger) eller strengmanipuleringsfunksjoner for å navigere gjennom HTML-strukturen. Avansert parsing kan kreve mer sofistikerte tilnærminger, inkludert egendefinerte parsingalgoritmer skreddersydd for den spesifikke strukturen av HTML du har med å gjøre, ettersom standard Arduino-miljøet ikke inkluderer et innebygget HTML-parsingbibliotek.
