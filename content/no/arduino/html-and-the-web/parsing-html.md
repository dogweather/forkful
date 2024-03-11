---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:38.987176-07:00
description: "Parsing av HTML i Arduino-prosjekter handler om \xE5 trekke ut informasjon\
  \ fra nettsider. Programmerere gj\xF8r dette for \xE5 muliggj\xF8re interaksjon\
  \ mellom deres\u2026"
lastmod: '2024-03-11T00:14:14.646884-06:00'
model: gpt-4-0125-preview
summary: "Parsing av HTML i Arduino-prosjekter handler om \xE5 trekke ut informasjon\
  \ fra nettsider. Programmerere gj\xF8r dette for \xE5 muliggj\xF8re interaksjon\
  \ mellom deres\u2026"
title: Analysering av HTML
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av HTML i Arduino-prosjekter handler om å trekke ut informasjon fra nettsider. Programmerere gjør dette for å muliggjøre interaksjon mellom deres Arduino-enheter og internett, ved å samle data fra nettsteder for formål som spenner fra hjemmeautomasjon til miljøovervåking.

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
