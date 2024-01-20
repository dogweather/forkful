---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Uuden projektin aloittaminen tarkoittaa uuden ohjelman tekemisen alkua tyhjästä. Ohjelmoijat tekevät sen, koska uusi projekti tarkoittaa uuden ongelman ratkaisemista tai uuden idean toteuttamista. 

## Näin se tehdään:

Aloitetaan yksinkertaisella LEDin vilkuttamisen ohjelmalla.

```Arduino
//Alustetaan LED pin
int ledPin = 13;

void setup() {
//Määritetään LED pinin toiminnallisuus
  pinMode(ledPin, OUTPUT); 
}

void loop() {
  digitalWrite(ledPin, HIGH);   //LED syttyy
  delay(1000);                  //Odotetaan sekunti
  digitalWrite(ledPin, LOW);    //LED sammuu
  delay(1000);                  //Odotetaan sekunti
}
```
Yllä olevan koodin abulla, LED vilkkuu syttymisen ja sammumisen välillä sekunnin välein.

## Deep Dive

Historiallisesti Arduino on avannut mahdollisuuden ohjelmoimiseen ja elektroniikkaprojektien kasaukseen kaikentasoisille innokkaille. Uuden projektin aloittaminen Arduinolla on yksinkertainen prosessi kiitos kätevän käyttöliittymän.

Vaihtoehdoista on hyvä mainita esimerkiksi Raspberry Pi, joka tarjoaa tehokkaamman alustan, mutta on samalla monimutkaisempi aloittelijoille.

Arduinossa uuden projektin aloittaminen liittyy yleensä LEDin vilkuttamiseen. Tämä johtuu siitä, että se on yksinkertainen tehtävä, joka auttaa käsittämään, kuinka koodi ohjaa laitteen käyttäytymistä.

## Katso myös

1. [Arduino viralliset oppaat](https://www.arduino.cc/en/Tutorial/HomePage)
2. [Raspberry Pi aloittelijoille](https://projects.raspberrypi.org/en/pathways/getting-started-with-raspberry-pi)
3. [31 DIY Arduino](https://www.instructables.com/DIY-Arduino-or-The-DIY-Duino/)