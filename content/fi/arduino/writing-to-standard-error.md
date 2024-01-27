---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Kirjoittaminen standardivirheeseen tarkoittaa virheilmoitusten ja muiden diagnostisten viestien tulostamista erilliseen virhekanavaan. Ohjelmoijat tekevät näin virheiden jäljittämiseen ja ohjelmien oikean toiminnan varmistamiseen.

## How to: - Kuinka tehdä:
Arduino-ympäristössä standardivirhekanavaa ei ole suoraan tarjolla, mutta voimme käyttää sarjaporttiviestintää (Serial) debuggaustarkoituksiin.

```Arduino
void setup() {
    Serial.begin(9600); // Avaa sarjaporttiyhteys nopeudella 9600 bps
}

void loop() {
    // Oletetaan, että funktio 'doSomething' voi epäonnistua
    bool result = doSomething();
    if(!result) {
        Serial.println("ERROR: 'doSomething' failed."); // Tulosta virheilmoitus sarjaporttiin
    }
}

bool doSomething() {
    // Funktio, joka voi epäonnistua
    // Simuloidaan epäonnistumista satunnaisesti
    return random(10) > 5;
}
```

Kun tämän ohjelman suorittaa, sarjaporttiin voidaan saada esimerkiksi viesti:

```
ERROR: 'doSomething' failed.
```

## Deep Dive - Syväsukellus:
Arduino-alustalla älykkäitä virheenkorjauskeinoja ei ole vakiona. Historiallisesti mikrokontrollerit eivät käyttäneet standardivirheeseen kirjoittamista, koska niillä ei ole käyttöjärjestelmän tarjoamaa monipuolista I/O-hallintaa. Vaihtoehtoisena debuggausmenetelmänä voidaan käyttää LED-merkkivaloja tai sarjaporttia, kuten yllä esimerkissä. Kirjoittaminen standardivirheeseen tapahtuu yleisesti tietokoneissa, joissa on käyttöjärjestelmät, jotka hallitsevat eri tulostuskanavia.

## See Also - Katso Myös:
- Arduino viralliset ohjelmointioppaat: https://www.arduino.cc/en/Guide
- Serial (Sarjaportti) -kommunikointi: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Debuggaus mikrokontrollereissa: https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent
