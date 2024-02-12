---
title:                "Verkkosivun lataaminen"
aliases:
- /fi/arduino/downloading-a-web-page.md
date:                  2024-01-20T17:43:33.042835-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä ja miksi? Ladataan web-sivu, eli haetaan tiedot internetissä olevasta osoitteesta. Koodarit tekevät tämän, kun tarvitsevat päivittää laitteen dataa tai käyttää etärajapintoja.

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
