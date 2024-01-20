---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Elm: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Komentoriviparametrien lukeminen tarkoittaa tietojen vastaanottamista käyttäjältä ohjelmaan. Tämä antaa ohjelmoijille mahdollisuuden lisätä joustavuutta ohjelmiinsa, säästäen samalla aikaa kompleksisten graafisten käyttöliittymien kehittämisessä.

## Kuinka Tehdään:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  while (Serial.available() > 0) {
    char c = Serial.read();  
    Serial.println(c);
  }
}
```
Yllä olevassa koodissa Arduino lukee saapuvat merkit sarjaportista ja tulostaa ne takaisin. Tätä voidaan käyttää komentoriviparametrien lukemiseen.

## Sukellus Syvemmälle

Alun perin komentoriviparametreja lukivat vain tekstipohjaiset ohjelmat, mutta nyt monimutkaisemmat graafiset ohjelmat voivat myös lukea ja käsitellä niitä. Vaihtoehtoisesti voidaan käyttää erilaisia tietoliikenneratkaisuja, kuten esimerkiksi TCP/IP-yhteyksiä parametrien siirtämiseen. Arduinon kannalta, komentoriviparametrien lukeminen toteutetaan lähinnä sarjaportin kautta.

## Katso Myös

Arduino-ohjelmointiin liittyvät linkit, mukaan lukien lisätietoja sarjaportin kautta tapahtuvasta komentoriviparametrien lukemisesta:

1. [Arduino Sarjaväylän Käyttö](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)
2. [Komentoriviparametrit C++ -ohjelmassa](https://www.cplusplus.com/articles/DEN36Up4/)