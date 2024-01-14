---
title:                "Arduino: Att analysera HTML"
simple_title:         "Att analysera HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Att hämta och tolka information från HTML-dokument kan vara en användbar funktion för ett Arduino-program. Det kan till exempel användas för att automatiskt hämta väderdata eller agera på kommandon från en hemsida.

## Hur man gör det

För att analysera HTML-dokument behövs det vissa speciella verktyg och bibliotek som kan läsas av Arduinoplattformen. Bland annat kan [Esp8266WiFi library](https://github.com/esp8266/Arduino) användas för att ansluta till internet och göra HTTP-förfrågningar.

För att tolka själva HTML-koden kan [ArduinoJson library](https://arduinojson.org/) användas. Detta bibliotek konverterar HTML-koden till en mer hanterbar JSON-struktur som kan bearbetas av Arduino-programmet.

Ett exempel på hur man kan använda dessa bibliotek är att hämta information om temperaturen från en väderhemsida. Koden skulle se ut på följande sätt:

```
// Inkludera nödvändiga bibliotek
#include <ESP8266WiFi.h>
#include <ArduinoJson.h>
 
// Anslut till WiFi 
const char* ssid = "minwifi";
const char* password = "12345678";
WiFiClient client;
 
// Ange URL till väderhemsidan
const char* url = "https://minväderhemsida.se/temperatur";
 
void setup() {
    Serial.begin(115200);
     
    // Anslut till WiFi
    Serial.print("Ansluter till ");
    Serial.println(ssid);
     
    WiFi.begin(ssid, password);
 
    // Vänta på anslutning
    while (WiFi.status() != WL_CONNECTED) {
        delay(500);
        Serial.print(".");
    }
    Serial.println("");
    Serial.println("WiFi ansluten");  
}
 
void loop() {
    // Gör en HTTP GET-förfrågan
    client.stop();
    if (client.connect("minväderhemsida.se", 80)) {
        client.println("GET /temperatur HTTP/1.1");
        client.println("Host: minväderhemsida.se");
        client.println("Connection: close");
        client.println();
 
        // Läs in hela svarstexten
        while(client.connected() && !client.available());
        while(client.connected() || client.available()) {
            String line = client.readStringUntil('\n');
 
            // Hitta starten på JSON-data
            if (line == "[") {
                // Skapa ett JSON-objekt
                DynamicJsonDocument doc(1024);
 
                // Läs in informationen
                while (client.connected() || client.available()) {
                    line = client.readStringUntil('\n');
                    DeserializationError error = deserializeJson(doc, line);
 
                    // Om det är en giltig rad, skriv ut temperaturen
                    if (!error) {
                        float temp = doc[1]["teplota"];
                        Serial.println("Temperaturen är " + String(temp));
                        break;
                    }
                }
            }
        }
    }
     
    delay(60000); // Hämta temperaturen varje minut
}
```

## Djupare dykning

Att tolka och hantera HTML-kod kan vara en komplex uppgift. Detta beror delvis på att HTML-dokument kan ha varierande struktur beroende på vilken hemsida man hämtar information ifrån. Det är därför viktigt att grundligt studera HTML-koden innan man börjar tolka den.

Man bör även vara medveten om att antalet element i JSON-strukturen som skapas av ArduinoJson library kan variera beroende på hur mycket information som hämtas från HTML-koden. Det är därför viktigt att ha en flexibel och dynamisk kod som kan hantera olika mängder av information.

## Se även

* [Esp8266WiFi library](https://github.com/esp8266/Arduino)
* [ArduinoJson library](https://arduinojson.org/)
* [HTML parsing tutorial för Arduino](https://randomnerdtutorials.com/esp8266-nodemcu-html-parser-tutorial/)