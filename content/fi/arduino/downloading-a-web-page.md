---
title:                "Arduino: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi: Web-sivun lataamisen hyödyt

Web-sivujen lataamisella on monia käyttötarkoituksia Arduinon ohjelmoinnissa. Esimerkiksi voit käyttää sitä hankkimaan tietoja Internetistä, päivittämään laitteesi ohjelmistoa tai yhdistämään sen verkkoon etäohjausta varten.

## Miten: Koodin esimerkki ja lähtö

Web-sivun lataaminen Arduinolle on helppoa käyttäen Ethernet-korttia tai WiFi-sovitinta. Alla olevassa koodiesimerkissä käytämme Ethernet-korttia ja tulostamme lataamamme web-sivun sisällön sarjaporttiin.

```Arduino
#include <Ethernet.h>
#include <SPI.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; // Arduinon Ethernet moduulin MAC-osoite
IPAddress ip(192, 168, 0, 5); // Arduinon IP-osoite
EthernetClient client;
char server[] = "www.esimerkkisivu.com"; // Lataamamme web-sivun osoite

void setup() {
  Ethernet.begin(mac, ip); // Alustetaan Ethernet-yhteys
  Serial.begin(9600); // Sarjaportin alustus
  delay(1000); // Pieni viive yhteyden muodostumiseen
  Serial.println("Yhdistetään palvelimeen...");
  delay(1000); // Pieni viive uudelleen
  if (client.connect(server, 80)) { // Yhdistetään web-palvelimeen portin 80 avulla
    Serial.println("Yhteys muodostettu");
    client.println("GET / HTTP/1.1"); // Lähetetään pyyntö GET-metodilla
    client.println("Host: www.esimerkkisivu.com"); // Pyydetään haluttu sivu
    client.println("Connection: close"); // Suljetaan yhteys latauksen jälkeen
    client.println(); // Tyhjä rivi
  }
}

void loop() {
  while (client.available()) { // Luetaan vastaanotettu data sarjaporttiin
    char c = client.read();
    Serial.print(c);
  }

  if (!client.connected()) { // Suljetaan yhteys ja odotetaan 5 sekuntia
    Serial.println();
    Serial.println("Lataus suoritettu");
    client.stop();
    while(true);
  }
}
```

Yllä olevassa koodiesimerkissä käytetään Ethernet-korttia, mutta WiFi-sovittimen kanssa koodi toimii hyvin samalla tavalla. Voit myös muokata koodia lataamaan muita web-sivuja vain muuttamalla osoitetta.

## Syvempi sukellus: Web-sivun lataaminen

Web-sivun lataaminen Arduinolle toimii protokollan nimeltä HTTP (Hypertext Transfer Protocol) avulla. HTTP käyttää yhteydessä TCP (Transmission Control Protocol) mahdollistaakseen tiedonsiirron web-palvelimen ja Arduinon välillä. Lataus alkaa lähettämällä pyyntö GET-metodilla web-palvelimelle, joka vastaa lähettämällä pyydetyn sivun tiedot takaisin.

HTTP muodostaa merkittävän osan Internetin toiminnasta ja on tärkeää ymmärtää sen periaatteet ja käytännöt, jos haluaa ladata web-sivuja Arduinolle tai tehdä muita Internet-yhteyksiä.

## Katso myös

- [Ethernet-kortin käyttö Arduinolla](https://www.arduino.cc/en/Reference/Ethernet)
- [WiFi-sovittimen käyttö Arduinolla](https://www.arduino.cc/en/Reference/WiFi)
- [HTTP ja TCP:n toimintaperiaatteet](https://developer.mozilla.org