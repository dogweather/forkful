---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:31.045771-07:00
description: "Att tolka HTML i Arduino-projekt handlar om att extrahera information\
  \ fr\xE5n webbsidor. Programmerare g\xF6r detta f\xF6r att m\xF6jligg\xF6ra f\xF6\
  r deras Arduino-enheter\u2026"
lastmod: 2024-02-19 22:04:57.400192
model: gpt-4-0125-preview
summary: "Att tolka HTML i Arduino-projekt handlar om att extrahera information fr\xE5\
  n webbsidor. Programmerare g\xF6r detta f\xF6r att m\xF6jligg\xF6ra f\xF6r deras\
  \ Arduino-enheter\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka HTML i Arduino-projekt handlar om att extrahera information från webbsidor. Programmerare gör detta för att möjliggöra för deras Arduino-enheter att interagera med internet, samla in data från webbplatser för ändamål som sträcker sig från hemautomation till miljöövervakning.

## Hur man gör:

Att tolka HTML på Arduino kräver vanligtvis bibliotek med minimalt fotavtryck på grund av begränsade enhetsresurser. Ett populärt val för webbskrapning och tolkning är att använda `ESP8266HTTPClient` och `ESP8266WiFi` biblioteken för ESP8266, eller deras motsvarigheter för ESP32, med tanke på deras inbyggda stöd för Wi-Fi-förmågor och HTTP-protokoll. Här är ett grundläggande exempel på att hämta och tolka HTML, med antagandet att du arbetar med en ESP8266 eller ESP32:

Först, inkludera de nödvändiga biblioteken:
```cpp
#include <ESP8266WiFi.h> // För ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Använd motsvarande ESP32-bibliotek om du använder en ESP32

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

Anslut till ditt Wi-Fi-nätverk:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Ansluter...");
    }
}
```

Gör en HTTP-förfrågan och tolka ett enkelt stycke HTML:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Kontrollera Wi-Fi-anslutningens status
        HTTPClient http;  //Deklarera ett objekt av klassen HTTPClient

        http.begin("http://example.com");  //Ange förfrågningsdestination
        int httpCode = http.GET();  //Skicka förfrågan

        if (httpCode > 0) { //Kontrollera returkoden
            String payload = http.getString();   //Hämta förfrågan responslast
            Serial.println(payload);             //Skriv ut responslasten

            // Tolka en specifik del, t.ex. extrahera titel från lasten
            int titleStart = payload.indexOf("<title>") + 7; // +7 för att flytta förbi "<title>"-taggen
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Sidtitel: ");
            Serial.println(pageTitle);
        }

        http.end();   //Stäng anslutning
    }

    delay(10000); //Gör en förfrågan var 10:e sekund
}
```

Exempel på utdata (med antagandet att http://example.com har en enkel HTML-struktur):
```
Ansluter...
...
Sidtitel: Exempeldomän
```

Detta exempel demonstrerar hämtning av en HTML-sida och extrahering av innehållet i `<title>`-taggen. För mer komplex HTML-tolkning, överväg att använda reguljära uttryck (med försiktighet på grund av minnesbegränsningar) eller strängmanipuleringsfunktioner för att navigera genom HTML-strukturen. Avancerad tolkning kan kräva mer sofistikerade tillvägagångssätt, inklusive skräddarsydda tolkningsalgoritmer anpassade till den specifika strukturen av HTML du hanterar, eftersom den standard Arduino-miljön inte inkluderar ett inbyggt HTML-tolkningsbibliotek.
