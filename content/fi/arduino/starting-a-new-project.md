---
title:                "Uuden projektin aloittaminen"
html_title:           "Arduino: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Aloittaminen uusi projekti on yksinkertainen prosessi, jossa luomme uuden ohjelman tai laitteen käyttäen Arduinoa. Ohjelmoijat tekevät tätä voidakseen luoda jotain uutta, tehdä harrastuksia tai vain harjoitella taitojaan.

## Miten:
Esimerkiksi voit aloittaa uuden Arduino-projektin seuraavalla tavalla: 
```Arduino
void setup() {
  // Asetetaan pinni 13 lähdöksi
  pinMode(13, OUTPUT); 
}

void loop() {
  digitalWrite(13, HIGH); // Sytytetään ledi
  delay(1000); // Pysäytetään ohjelma 1 sekunniksi
  digitalWrite(13, LOW); // Sammutetaan ledi
  delay(1000); // Pysäytetään ohjelma 1 sekunniksi
}
```
Tämä koodi asettaa liittimen 13 lähdöksi ja käyttää sitä ohjaamaan lediä. Ohjelma sytyttää ledin sekunnin ajaksi ja sammuu sitten sekunnin ajaksi. Tämä toistuu loputtomiin.

## Syvällinen sukellus:
Projektien aloittaminen Arduinoa käyttäen on suosittua johtuen sen helppokäyttöisyydestä ja laajasta yhteisöstä. Vaihtoehtoisia laitteistoja ja ohjelmointiympäristöjä on olemassa, mutta Arduino on suosittu erityisesti harrastajien keskuudessa. Prosessi alkaa yleensä suunnittelusta ja jatkuu koodauksella, testauksella ja viimeistelyllä.

## Katso myös:
Löydät lisätietoja Arduino-projektien aloittamisesta seuraavista lähteistä:
- [Arduino verkkosivut](https://www.arduino.cc/en/Guide/HomePage)
- [Official Arduino Blog](https://blog.arduino.cc/)
- [Arduino Projektit](https://create.arduino.cc/projecthub)