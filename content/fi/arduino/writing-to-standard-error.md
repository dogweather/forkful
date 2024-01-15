---
title:                "Kirjoittaminen standardivirheelle"
html_title:           "Arduino: Kirjoittaminen standardivirheelle"
simple_title:         "Kirjoittaminen standardivirheelle"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Arduino ohjelmoinnissa voi joskus tulla vastaan virheitä ja ongelmia, joita tarvitaan korjaamaan. Tämä voi johtaa tarpeeseen kirjoittaa viestiä "standard error" -tulostukseen, jolla voidaan lisätä ohjelman virheen selvitystä ja helpottaa sen korjaamista.

## Kuinka Kirjoittaa Standard Error Viestejä Arduinossa

Kun haluat kirjoittaa viestin standard error -tulostukseen Arduinossa, käytä seuraavaa koodia:
```Arduino
Serial.println("Tämä on standard error viesti.");
```
Tämä koodi tulostaa tekstin "Tämä on standard error viesti." Arduinon sarjaporttiin, jota voit seurata ohjelman suorituksen aikana.

Voit myös lisätä muuttujia viestiin käyttämällä seuraavaa koodia:
```Arduino
int luku = 5;
Serial.println("Luku on " + String(luku));
```
Tässä esimerkissä "luku" -muuttuja lisätään viestiin ja sen arvo tulostetaan.

## Syvemmälle

Standard error -tulostuksen käyttäminen voi auttaa pääsemään nopeasti jäljille ohjelman virheille ja helpottaa niiden korjaamista. Voit myös käyttää "Serial.print()" -toimintoa, joka ei lisää automaattisesti rivinvaihtoa viestiin. Tämä on hyödyllistä esimerkiksi silloin, kun haluat lisätä useita muuttujia samaan viestiin.

## Katso Myös

- [Arduino Reference - Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Instructables - Debugging Arduino Code with Serial Prints](https://www.instructables.com/Debugging-Arduino-Code-With-Serial-Prints/)