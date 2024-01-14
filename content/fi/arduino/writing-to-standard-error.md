---
title:                "Arduino: Kirjoittaminen standardivirheelle"
simple_title:         "Kirjoittaminen standardivirheelle"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi: Miksi kirjoittaa standardierroriin?

Kirjoittaminen standardierroriin on hyödyllinen tapa havaita ja korjata ohjelmointivirheitä Arduino-projektissa. Se on myös hyvä tapa varmistaa, että ohjelmasi toimii oikein ja ettei se kaadu odottamatta.

## Miten: Esimerkkejä ja koodilohkoja

```Arduino
// Esimerkki kirjoittamisesta standardierroriin
Serial.print("Tämä on virheellinen lause"); // Tulostaa "Tämä on virheellinen lause" standardierroriin
```

Kun kirjoitat standardierroriin, ohjelma kirjoittaa viestit vakiotulostovirhevirtaan, jota voidaan seurata sarjaportin monitorissa. Tämä auttaa vianetsintään ja virheiden tunnistamiseen.

```Arduino
// Esimerkki standardierroriin kirjoittamisesta ja virheen käsittelystä
Serial.print("Tervetuloa Arduino-projektiin!"); // Tulostaa tervehdyksen
Serial.print("Tämä on virheellinen lause"); // Tulostaa "Tämä on virheellinen lause" standardierroriin

// Virheenkäsittelijä
void errorHandling() {
  // Käsittelee virheen
}

void setup() {
  // Alustaa sarjaportin monitorin
  Serial.begin(9600);
}

void loop() {
  // Suorittaa päätoiminnot
  errorHandling(); // Kutsuu virheenkäsittelijää, jos virhe tapahtuu
}
```

## Syvempää tietoa standardierroriin kirjoittamisesta

Standardierroriin kirjoittamista käytetään usein yhdessä vianetsintätoimintojen, kuten Serial.print(), kanssa. Se on myös hyödyllistä käyttää, kun haluat vain kirjoittaa tietyn viestin näkyviin, kun ohjelman suoritus on päättynyt. Huomaa, että standardierroriin kirjoittaminen ei katkaise ohjelman suoritusta, vaan se jatkaa suoritustaan normaalisti.

## Katso myös

- [Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino - kommunikointi sarjaportin kanssa](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialCommunication)
- [Vianetsintä Arduino-projekteissa](https://create.arduino.cc/projecthub/Anyintelli/how-to-troubleshoot-arduino-projects-7d66ff)