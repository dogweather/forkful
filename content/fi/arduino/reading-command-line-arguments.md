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

## Miksi

Yleensä, kun kirjoitat Arduinon koodia, sinulla on joitain muuttujia, joihin käyttäjän tarvitsee syöttää tietoja. Tämä onnistuu helposti laitteen pinnien tai näppäimistön avulla, mutta jos haluat antaa käyttäjän syöttää tietoja jo ennen kuin laite käynnistetään, tarvitaan jotain muuta. Tämä "jotain muuta" voi olla komentoriviparametrit, joilla pystytään ohjaamaan laitteen toimintaa jo ennen kuin koodi suoritetaan.

## Miten

Komentoriviparametrien lukeminen Arduinolla onnistuu helposti käyttämällä ```Arduino.h``` kirjastoa ja siihen sisältyvää ```Serial``` kirjastoa. Seuraa alla olevia esimerkkejä saadaksesi paremman käsityksen siitä, miten komentoriviparametreja käytetään koodissa.

```Arduino
#include <Arduino.h>
#include <Serial.h>

void setup() {
    Serial.begin(9600); // käynnistetään sarjayhteys
    while (!Serial) {
        // odotetaan, kunnes sarjayhteys on muodostettu
    }
}

void loop() {
    if (Serial.available()) { // tarkistetaan, onko sarjayhteys avoinna
        String input = Serial.readString(); // luetaan seriaalipuskuriin tullut data
        Serial.println("Komentoriviparametrit ovat: " + input); // tulostetaan saatu data
    }
}
```

Esimerkin koodi lukee sarjayhteyden kautta Arduinolle lähetettyjä komentoriviparametreja ja tulostaa ne sarjayhteyden kautta. Nyt voit antaa Arduinolle komentoriviparametreja ennen sen käynnistämistä ja laite suorittaa niiden mukaisen toiminnon.

```bash
$ arduino-cli upload -p COM3 -b arduino:avr:uno
```

Yllä olevassa komennossa lähetetään Arduinon koodi portille ```COM3``` ja käytetään ```arduino:avr:uno``` alusta.

## Syvempi sukellus

Komentoriviparametrien lukeminen Arduinolla käyttää hyväksi mikro-ohjaimen integroitua sarjayhteyttä. Tämän ansiosta voit kommunikoida laitteen kanssa esimerkiksi ```Serial.print()``` ja ```Serial.read()``` -komennoilla. Muista myös, että voidaksesi käyttää Arduinon komentoriviparametrejä, sinun tulee ensin avata sarjayhteys koodissasi.

## Katso myös

- [Arduino-hallintakonsoli](https://www.arduino.cc/en/Main/Software)
- [Arduino-hallintatiedostot](https://github.com/arduino/Arduino/tree/master/build/shared/examples)