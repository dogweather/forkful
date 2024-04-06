---
date: 2024-01-20 17:43:33.042835-07:00
description: "How to: Sukellus syvemm\xE4lle: Historiallisesti Arduinot eiv\xE4t olleet\
  \ yhteydess\xE4 internetiin. ESP8266-moduulin tai Ethernet-shieldin kaltaiset laajennukset\u2026"
lastmod: '2024-04-05T22:51:10.973358-06:00'
model: gpt-4-1106-preview
summary: "Sukellus syvemm\xE4lle."
title: Verkkosivun lataaminen
weight: 42
---

## How to:
Miten se tehdään:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "sinunSSID";
const char* password = "sinunSalasana";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while(WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Yhdistetään WiFi-verkkoon...");
  }

  Serial.println("WiFi yhdistetty!");
  Serial.print("IP-osoite: ");
  Serial.println(WiFi.localIP());
}

void loop() {
  if(WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    
    http.begin("http://example.com"); // Web-sivusi URL
    int httpCode = http.GET();
    
    if(httpCode > 0) {
      if(httpCode == HTTP_CODE_OK) {
        String payload = http.getString();
        Serial.println(payload);
      }
    } else {
      Serial.println("Virhe HTTP-pyynnössä");
    }
    
    http.end();
  }
  
  delay(10000); // Odota 10 sekuntia ennen seuraavaa pyyntöä
}
```

## Deep Dive
Sukellus syvemmälle:

Historiallisesti Arduinot eivät olleet yhteydessä internetiin. ESP8266-moduulin tai Ethernet-shieldin kaltaiset laajennukset muuttivat tilanteen. Nyt voit ladata web-sivuja suoraan ohjelmoimaltasi laitteelta.

Vaihtoehtoja HTTPClient-kirjastolle ovat esimerkiksi WebClient tai käyttämällä matalamman tason TCP/UDP-protokollia. Käyttämäsi kirjasto määräytyy tarpeesi, kuten muistin, suorituskyvyn tai erityistoimintojen, mukaan.

Tekninen toteutus vaatii verkkoyhteyden hallintaa ja HTTP-protokollan ymmärrystä. Käytettäessä GET-metodia pyydetään tietoa palvelimelta. Saatua vastausta voidaan käsitellä esimerkiksi jäsentämällä HTML tai JSON-muodossa.

## See Also
Katso myös:

- [ESP8266WiFi-kirjaston dokumentaatio](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [Arduinon Ethernet-kirjaston dokumentaatio](https://www.arduino.cc/en/Reference/Ethernet)
