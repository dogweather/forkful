---
title:                "Web-sivun lataaminen"
html_title:           "Arduino: Web-sivun lataaminen"
simple_title:         "Web-sivun lataaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Web-sivujen lataaminen on tärkeä osa nykyaikaista teknologiaa, ja Arduino tarjoaa helpon ja joustavan tavan toteuttaa tämä ominaisuus omassa projektissasi. Lataamalla web-sivuja voit esimerkiksi näyttää reaaliaikaista tietoa projektiisi liittyen tai saada päivityksiä ulkoisilta lähteiltä.

## Miten

Web-sivujen lataaminen Arduino-ohjelmassa onnistuu käyttämällä WiFi-yhteyttä ja HTTP GET -pyyntöjä. Ensiksi tarvitset WiFi-moduulin, kuten ESP8266, ja sen jälkeen voit käyttää esimerkiksi ESP8266WiFi-kirjastoa kommunikoidaksesi verkossa. Alla on yksinkertainen esimerkki, joka lataa ja tulostaa HTML-sisällön Arduino Serial Monitoriin.

```Arduino
#include <ESP8266WiFi.h>

const char* ssid = "WiFi-verkon-nimi";
const char* password = "WiFi-salasana";

void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, password);
  
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Yhdistetään WiFi-verkkoon...");
  }

  Serial.println("WiFi-yhteys muodostettu!");
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin("http://www.esimerkkisivu.fi/"); // Muuta osoite haluamaksesi
    int httpCode = http.GET();

    if (httpCode > 0) {
      Serial.println("Web-sivu ladattu.");
      String html = http.getString();
      Serial.println(html); // Tulostaa sivun sisällön
    }
    else {
      Serial.println("Virhe ladattaessa web-sivua.");
    }
    
    http.end();
  }
  else {
    Serial.println("WiFi-yhteys katkaistu.");
  }

  delay(5000); // Lataa web-sivua uudelleen 5 sekunnin välein
}
```

## Syvällisempi tarkastelu

HTTP GET -pyyntöä käytetään lähettämään tiettyä tietoa web-sivulta palvelimelle ja saamaan vastauksena tietoa palvelimen puolelta. Tämä toimii hyvin yksinkertaisten web-sivujen lataamisessa, mutta vaativammissa projekteissa voi olla tarpeen käyttää esimerkiksi REST API:a tai muita kommunikointitapoja. On myös tärkeää muistaa, että WiFi-yhteys voi olla epäluotettava ja pyyntöjä tulee käsitellä virheiden varalta.

## Katso myös

- [Arduino HTTPClient library](https://github.com/arduino-libraries/ArduinoHttpClient)
- [ESP8266WiFi library reference](https://arduino-esp8266.readthedocs.io/en/2.6.0/esp8266wifi/readme.html)
- [REST API tutorial](https://www.restapitutorial.com/lessons/whatisrest.html)