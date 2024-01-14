---
title:                "Arduino: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi
Arduino on ohjelmointialusta, jota käytetään monipuolisten projektejen toteuttamiseen. HTTP-pyyntöjen lähettäminen Arduinoon voi olla hyödyllistä esimerkiksi tiedonkeruuta tai etäkäyttöä varten.

## Miten
HTTP-pyynnöt voidaan lähettää Arduinolle käyttämällä Ethernet- tai WiFi-yhteyttä ja kirjastoa nimeltä "HTTPClient.h". Seuraavassa esimerkissä lähetämme GET-pyynnön Arduino Uno -laitteelle ja tulostamme vastauksen sarjaporttiin:

```Arduino
#include <SPI.h>
#include <Ethernet.h>
#include <HTTPClient.h>

// määritellään Ethernet-yhteys
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192, 168, 1, 177);

EthernetClient client;

void setup() {
  // avataan sarjaportti yhteyttä varten
  Serial.begin(9600);
  
  // käynnistetään Ethernet-yhteys
  Ethernet.begin(mac, ip);
  
  // odotetaan, että yhteys muodostetaan
  delay(1000);

  // luodaan HTTPClient-olio
  HTTPClient http;
  
  // lähetetään GET-pyyntö ja tallennetaan vastaus muuttujaan
  http.begin("http://www.example.com");
  int status = http.GET();
  String response = http.getString();
  
  // suljetaan yhteys
  http.end();
  
  // tulostetaan vastaus sarjaporttiin
  Serial.println(response);
}

void loop() {
  // ei tehdä mitään jatkuvasti
}
```

Yllä oleva koodi lähettää GET-pyynnön osoitteeseen "http://www.example.com" ja tallentaa vastauksen muuttujaan. Sen jälkeen se tulostaa vastauksen sarjaporttiin.

## Syventävä tarkastelu
HTTP-pyyntöjä voidaan lähettää myös käyttämällä muita kirjastoja, kuten "ESP8266HTTPClient.h" tai "WiFiClient.h". Näitä kirjastoja käytetään yleensä, kun käytetään Arduinon WiFi-yhteyttä. Lisäksi HTTP-pyynnöillä voidaan lähettää myös dataa ja vastaanottaa vastauksia eri muodoissa, kuten JSON tai XML.

## Katso myös
- [Ethernet-kirjasto Arduino.cc:ssä](https://www.arduino.cc/en/Reference/Ethernet)
- [WiFi-kirjasto GitHubissa](https://github.com/arduino-libraries/WiFi)
- [HTTPClient-kirjasto GitHubissa](https://github.com/arduino-libraries/ArduinoHttpClient)