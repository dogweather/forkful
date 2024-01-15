---
title:                "Lähettämässä http-pyyntöä perusautentikointilla"
html_title:           "Arduino: Lähettämässä http-pyyntöä perusautentikointilla"
simple_title:         "Lähettämässä http-pyyntöä perusautentikointilla"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lähettää HTTP-pyynnön perusautentikoinnilla? Yksinkertaisesti, HTTP-pyynnöt ovat niiden avulla tapa lähettää tietoja verkon yli ja perusautentikointi antaa meille mahdollisuuden suojata tietoja. Tämä on erityisen tärkeää, kun liikenne kulkee julkisen verkon kautta, kuten Internetissä.

## Miten

Seuraavassa on esimerkki kuinka voit lähettää HTTP-pyynnön perusautentikoinnilla käyttäen Arduinon HTTPClient kirjastoa:

```
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "WiFi-verkon-nimi"; 
const char* password = "WiFi-verkon-salasana";

void setup() {

  Serial.begin(9600);
  WiFi.begin(ssid, password); // Muodosta yhteys WiFi-verkkoon
  Serial.println("Trying to connect to WiFi...");
  while (WiFi.status() != WL_CONNECTED) { // Odota yhteys WiFi-verkkoon 
    delay(500);
    Serial.print(".");
  }
  Serial.println("Connected to WiFi!");

  // Luo HTTP-objekti perusautentikoinnilla
  HTTPClient http;
  http.begin("http://www.example.com", "username", "password");

  int httpCode = http.GET(); // Lähetä GET-pyyntö
  
  if (httpCode > 0) { // Tarkista vastaus
    String response = http.getString();
    Serial.println(response); // Tulosta vastaus
  }
  else {
    Serial.println("Error on HTTP request");
  }

  http.end(); // Sulje yhteys
}

void loop() {

}
```

Esimerkissä luomme ensin yhteyden WiFi-verkkoon, jonka jälkeen luomme HTTP-objektin käyttäen osoitetta, käyttäjänimeä ja salasanaa. Sitten lähetämme GET-pyynnön ja tulostamme vastauksen sarjaporttiin. Lopuksi suljemme yhteyden.

## Syventävä sukellus

Perusautentikoinnissa käyttäjänimet ja salasanat lähetetään selkeässä tekstimuodossa, joka on altis väärinkäytöksille. Vahvemman turvallisuuden takaamiseksi suosittelemme käyttämään SSL/TLS-suojattua yhteyttä tai muita turvallisia autentikointimenetelmiä.

## Katso myös

- [ESP8266WiFi kirjasto (ESP8266:n WiFi-toiminnallisuuden ohjaus)](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [ESP8266HTTPClient kirjasto (HTTP-yhteyksien hallinta)](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)