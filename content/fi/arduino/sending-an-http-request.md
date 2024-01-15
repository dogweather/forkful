---
title:                "Lähetetään http-pyyntö"
html_title:           "Arduino: Lähetetään http-pyyntö"
simple_title:         "Lähetetään http-pyyntö"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Miksi

Miksi haluaisit lähettää HTTP-pyynnön Arduino-piirissä? Yksinkertaisesti sanottuna, se on kätevä tapa kommunikoida internetin ja muiden laitteiden kanssa ja saada tietoa tai suorittaa toimintoja kauko-ohjauksella.

# Miten

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

const char* ssid = "wifi-verkon-nimi";
const char* password =  "wifi-verkon-salasana";
const char* server = "murmeliporno.com";  // Vaihda osoite oman verkkosi mukaan

void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
}

void loop() {
  if(WiFi.status() == WL_CONNECTED){ // varmistetaan, että WiFi-yhteys on aktiivinen
    HTTPClient http; // luodaan HTTP-yhteysolio
    String endpoint = "/api/temperature";
    http.begin("http://" + String(server) + endpoint);  // yhdistetään osoite ja endpoint

    int httpCode = http.GET(); // lähetetään HTTP GET -pyyntö
    if(httpCode == 200) { // vastaus 200 = OK
      String response = http.getString(); // tallennetaan vastaus muuttujaan
      Serial.println(response); // tulostetaan vastaus sarjaporttiin
    } else {
      Serial.println("Virhe pyynnössä"); // jos vastaus ei ole OK, tulostetaan virheilmoitus
    }
    http.end(); // suljetaan HTTP-yhteys
  }
  delay(30000); // odotetaan 30 sekuntia ennen seuraavaa pyyntöä
}
```

*Output:*

```
22.3 Celsius
```

## Deep Dive

HTTPClient-kirjasto tarjoaa meille helpon tavan lähettää HTTP-pyyntöjä ja vastaanottaa vastauksia. Koodissa ensin luodaan WiFi-yhteys ja varmistetaan, että se on aktiivinen. Sitten luodaan HTTPClient-olio ja käytetään sen begin() -metodia määrittämään yhteysosoite ja endpoint-polut. Lopuksi lähetetään pyyntö GET-metodilla ja tallennetaan vastaus string-muuttujaan. On myös tärkeää sulkea yhteys lopuksi end()-metodilla. 

## Katso myös

- [WiFi kirjasto](https://www.arduino.cc/en/Reference/WiFi)
- [HTTPClient kirjasto](https://www.arduino.cc/en/Reference/HTTPClient)
- [HTTP-pyyntöjen lähetys ja vastaanotto Arduino-piirillä](https://randomnerdtutorials.com/esp32-dht11-dht22-temperature-humidity-web-server-arduino-ide/)