---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
aliases:
- fi/arduino/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:00:50.771459-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
HTTP-pyyntö perusautentikoinnilla lähettää tietoa palvelimelle suojattuna. Se on turvallinen tapa päästä käsiksi API:hin tai palveluun, joka vaatii käyttäjätunnistusta.

## How to: (Kuinka tehdä:)
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
const char* serverName = "http://yourserver.com";
const char* httpUsername = "user";
const char* httpPassword = "pass";

void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);
    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Connecting to WiFi...");
    }

    HTTPClient http;
    http.begin(serverName);
    http.setAuthorization(httpUsername, httpPassword);
    int httpResponseCode = http.GET();

    if (httpResponseCode > 0) {
        String response = http.getString();
        Serial.println(httpResponseCode);
        Serial.println(response);
    } else {
        Serial.print("Error on sending request: ");
        Serial.println(httpResponseCode);
    }
    http.end();
}

void loop() {
    // nothing here
}
```
Tulostaa vastauskoodin ja palvelimen vastauksen, tai virhekoodin jos pyyntö epäonnistuu.

## Deep Dive (Syväsukellus)
Perusautentikointi on HTTP-protokollan mekanismi, jossa käyttäjätunnus ja salasana lähetetään Base64-koodattuna. Se on yksinkertainen ja melko vanha, mutta yhä käytössä pienten projektien turvallisuutta parantamaan. Vaihtoehtona on esimerkiksi OAuth, mutta perusautentikointi on nopea ja kätevä pienille projekteille. Kun lähetät HTTP-pyyntöä Arduinolla, käytät usein ESP8266/ESP32-kaltaisia moduuleja, jotka hoitavat WiFi-yhteyden ja HTTP-asiakaslogiikan.

## See Also (Katso Myös)
- ESP8266HTTPClient-kirjaston dokumentaatio: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/http-client.html
- HTTP-autentikoinnin yleiskatsaus: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Base64-koodauksen selitys: https://en.wikipedia.org/wiki/Base64
