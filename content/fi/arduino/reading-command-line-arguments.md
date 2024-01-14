---
title:                "Arduino: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi: Miksi lukea komentorivi argumentteja?

Komentorivi argumentit ovat tärkeitä, koska ne mahdollistavat Arduinon ohjelmoijille monipuolisen ja joustavan tavan antaa komentoja ja asetuksia ohjelmalle. Tämä on erityisen hyödyllistä, kun halutaan muuttaa ohjelman toimintaa tai parametreja ilman, että koodia täytyy muokata ja ladata Arduinoon uudelleen.

## Kuinka: Koodiesimerkkejä ja tulosteita

Komentorivi argumenttien lukeminen Arduinolla on yksinkertaista. Se tapahtuu käyttämällä Arduino IDE:n sisäänrakennettuja funktioita `argc` ja `argv`. Tässä esimerkissä luetaan komentorivi argumentti ja tulostetaan se Arduino sarjaporttiin:

```Arduino
void setup() {
  Serial.begin(9600); // alustaa sarjaportin nopeudella 9600 bps
  Serial.println(argv[0]); // tulostaa komentorivi argumentin
}

void loop() {
  // ei tehdä mitään
}
```

Kun tämä koodi ladattaan Arduinoon ja ohjelma käynnistetään, sarjaporttiin tulostuu ensimmäinen komentorivi argumentti, joka annettiin käynnistettäessä. Esimerkiksi jos komentoriville kirjoitettaisiin `arduino-cli sketch -c uno`, sarjaporttiin tulostettaisiin `sketch`.

## Syvemmälle: Tietoa komentorivi argumenttien lukemisesta

Komentorivi argumentit annetaan ohjelmalle käynnistettäessä ja ne tallentuvat muuttujiin `argc` (argument count) ja `argv` (argument vector). Muuttujassa `argc` on tallennettuna komentorivi argumenttien lukumäärä, ja muuttujassa `argv` on taulukko merkkijonoja, joissa jokainen taulukon alkio vastaa yhtä komentorivi argumenttia.

On tärkeää muistaa, että argumentit tallennetaan taulukkoon merkkijonoina, joten ne täytyy muuttaa sopivaan tyyppiin, jos niitä halutaan käyttää esimerkiksi laskutoimituksissa.

## Katso myös

- Arduino `argc` ja `argv` dokumentaatio: https://www.arduino.cc/reference/en/language/functions/io/argc/
- Arduino-esimerkki komentorivi argumenttien lukemisesta: https://www.arduino.cc/en/Tutorial/CommandLineArguments
- Arduino CLI - ohjelman käyttöohjeet: https://arduino.github.io/arduino-cli/latest/