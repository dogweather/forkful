---
title:                "HTML:n jäsennys"
aliases:
- fi/arduino/parsing-html.md
date:                  2024-02-03T19:11:32.723041-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML:n jäsentäminen Arduino-projekteissa koskee tietojen poimimista verkkosivuilta. Ohjelmoijat tekevät tämän mahdollistaakseen Arduinolaitteidensa vuorovaikutuksen internetin kanssa, keräten tietoja verkkosivustoilta tarkoituksiin, jotka vaihtelevat kotiautomaatiosta ympäristön seurantaan.

## Kuinka:

HTML:n jäsentäminen Arduinolla vaatii yleensä pienikokoisia kirjastoja rajoitettujen laiteresurssien vuoksi. Suosittu valinta verkkosivujen kaapimiseen ja jäsentämiseen on käyttää `ESP8266HTTPClient`- ja `ESP8266WiFi`-kirjastoja ESP8266:lle, tai niiden ESP32-vastineita, ottaen huomioon niiden natiivin tuen Wi-Fi-ominaisuuksille ja HTTP-protokollille. Tässä on perusesimerkki HTML:n noutamisesta ja jäsentämisestä, olettaen että käytät ESP8266:ta tai ESP32:ta:

Ensiksi, sisällytä tarvittavat kirjastot:
```cpp
#include <ESP8266WiFi.h> // ESP8266 varten
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Käytä vastaavia ESP32-kirjastoja jos käytät ESP32:ta

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

Yhdistä Wi-Fi-verkkoosi: 
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Yhdistetään...");
    }
}
```

Tee HTTP-pyyntö ja jäsenne yksinkertainen HTML-koodinpätkä:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Tarkista Wi-Fi-yhteyden tila
        HTTPClient http;  //Julista HTTPClient-luokan olio

        http.begin("http://example.com");  //Määritä pyynnön kohde
        int httpCode = http.GET();  //Lähetä pyyntö

        if (httpCode > 0) { //Tarkista palautuva koodi
            String payload = http.getString();   //Hae pyynnön vastauskuorma
            Serial.println(payload);             //Tulosta vastauskuorma

            // Jäsennä tietty osa, esim. otsikon poimiminen kuormasta
            int titleStart = payload.indexOf("<title>") + 7; // +7 siirtyäkseen "<title>"-tagin ohi
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Sivun otsikko: ");
            Serial.println(pageTitle);
        }

        http.end();   //Sulje yhteys
    }

    delay(10000); //Tee pyyntö joka 10. sekunti
}
```

Esimerkkitulostus (olettaen, että http://example.com sisältää yksinkertaisen HTML-rakenteen):
```
Yhdistetään...
...
Sivun otsikko: Esimerkkialue
```

Tämä esimerkki osoittaa HTML-sivun noutamisen ja `<title>`-tagin sisällön poimimisen. Monimutkaisempaa HTML-jäsentämistä varten harkitse säännöllisten lausekkeiden käyttöä (varoen muistirajoituksia) tai merkkijonojen käsittelyfunktioita navigoidaksesi läpi HTML-rakenteen. Edistynyt jäsentäminen saattaa vaatia monimutkaisempia lähestymistapoja, mukaan lukien räätälöidyt jäsentämisalgoritmit, jotka on suunniteltu erityisen HTML-rakenteen käsittelyyn, koska standardi Arduino-ympäristö ei sisällä valmista HTML-jäsentämiskirjastoa.
