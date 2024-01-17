---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Arduino: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Komentoriviparametrien lukeminen on tapa saada ulkoisia tietoja koodiin. Ohjelmoijat tekevät sitä usein käyttäjän antamien arvojen tai toimintojen hallitsemiseksi.

## Miten:
Esimerkkejä koodista ja tulosteesta, ```Arduino ...``` -lohkoissa.

Esimerkki: 
```Arduino
int merkki; 

void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available() > 0) {
    merkki = Serial.read(); 
    Serial.print("Käyttäjän antama merkki: ");
    Serial.println(merkki); 
  }
}
```

Tuloste:

Käyttäjän antama merkki: X

## Syväsukellus:
Kommentoriviparametrien lukeminen on ollut käytössä ohjelmoinnissa jo pitkään. Se on yleinen tapa kommunikoida käyttäjän kanssa ja antaa heille mahdollisuus ohjata ohjelmaa.

On olemassa myös muita tapoja saada ulkoisia tietoja koodiin, kuten tiedostojen lukeminen tai käyttäjän kanssa vuorovaikutuksessa olevan käyttöliittymän luominen.

Kommentoriviparametrien lukeminen on mahdollista myös monilla muilla ohjelmointikielillä, ei vain Arduino-ympäristössä.

## Katso myös:
- [Arduino Language Reference - Serial.read()](https://www.arduino.cc/reference/en/language/functions/communication/serial/read/)
- [Wikipedia - Command-line interface](https://en.wikipedia.org/wiki/Command-line_interface) 
- [The C Programming Language](https://www.amazon.com/Programming-Language-2nd-Brian-Kernighan/dp/0131103628)