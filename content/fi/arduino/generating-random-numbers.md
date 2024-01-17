---
title:                "Sattumanvaraisten numeroiden luominen"
html_title:           "Arduino: Sattumanvaraisten numeroiden luominen"
simple_title:         "Sattumanvaraisten numeroiden luominen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Ohjelmoijat käyttävät satunnaislukujen tuottamista monissa projekteissa, joissa tarvitaan sattumanvaraista toimintaa tai epäjatkuvuutta. Satunnaislukujen tuottaminen on prosessi, jossa ohjelma luo arvoja ilman määrättyä järjestystä tai ennustettavuutta.

## Miten:
Satunnaislukujen tuottaminen on helppoa ja nopeaa Arduino-ohjelmoinnissa. Voit käyttää sisäänrakennettua random() -funktiota saadaksesi satunnaisen luvun tietyltä väliltä. Voit myös käyttää analogRead() -funktiota hyödyntääksesi ulkoista satunnaislukugeneraattoria. Katso alla olevat esimerkit ja tulosteet.

```Arduino
// Esimerkki 1: Random-funktio

void setup() {
  Serial.begin(9600); // alustaa sarjaportin
}

void loop() {
  int randomNum = random(0,100); // luo satunnaisen luvun väliltä 0-99
  Serial.println(randomNum); // tulostaa luvun sarjaportille
  delay(1000); // odota sekunti ennen uuden luvun luomista
}
```
Tuloste: 34, 77, 12, 93, 45, 0, 99, 65, 21, 48, 8, ...

```Arduino
// Esimerkki 2: analogRead-funktio

void setup() {
  Serial.begin(9600); // alustaa sarjaportin
}

void loop() {
  int sensorValue = analogRead(A0); // lukee analogisen signaalin A0-portista
  int randomNum = map(sensorValue, 0, 1023, 0, 100); // muuntaa luvun välille 0-100
  Serial.println(randomNum); // tulostaa luvun sarjaportille
  delay(1000); // odota sekunti ennen uuden luvun luomista
}
```
Tuloste: 45, 77, 12, 82, 34, 93, 2, 68, 99, 23, 48, ...

## Syvemmälle:
Satunnaislukujen tuottamisella on ollut merkittävä rooli tietokoneohjelmoinnissa ja matematiikassa jo vuosikymmenten ajan. Ennen tietokoneita, satunnaislukuja tuotettiin fyysisillä mekanismeilla, kuten noppien tai arpapallojen avulla. Nykyään satunnaislukuja tuotetaan yleisesti tietokoneella erilaisilla algoritmeilla.

On myös muita tapoja tuottaa satunnaisia lukuja Arduinossa, kuten käyttämällä aiemmin mainittujen lisäksi millis() - ja randomSeed() -funktioita. Jokaisella näistä tavoista on omat etunsa ja haittansa, joten kannattaa tutustua niihin tarkemmin tarpeen mukaan.

## Katso myös:
- Arduino:n virallinen dokumentaatio satunnaislukujen tuottamisesta: https://www.arduino.cc/reference/en/language/functions/random-numbers/
- Satunnaislukugeneraattorien käyttö tietokoneohjelmoinnissa: https://en.wikipedia.org/wiki/Random_number_generation