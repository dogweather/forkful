---
title:                "Lukeminen teksti-tiedostosta"
html_title:           "Arduino: Lukeminen teksti-tiedostosta"
simple_title:         "Lukeminen teksti-tiedostosta"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Arduino on suosittu alusta monien erilaisten projektien toteuttamiseen. Yksi hyödyllinen taito Arduino-ohjelmoinnissa on kyky lukea tekstitiedostoja. Tässä artikkelissa opit miten lukea tekstitiedostoja Arduino-koodilla.

## Miten

```Arduino
// Avataan tiedosto lukemista varten
File tiedosto = SD.open("tiedosto.txt");

// Luetaan tiedostosta rivi kerrallaan
while(tiedosto.available()){
  Serial.println(tiedosto.readStringUntil('\n'));
}
```

Seuraava Arduino-koodi esimerkki näyttää kuinka avata ja lukea tekstitiedosto SD-kortilta. Ensimmäisessä rivissä avataan tiedosto lukemista varten ja tallennetaan se muuttujaan `tiedosto`. Tämän jälkeen `while`-silmukan avulla käydään tiedosto läpi ja `readStringUntil()` -funktiolla luettaan rivi kerrallaan ja tulostetaan se sarjaporttiin `Serial.println()` avulla.

Jos haluat lukea vain tietyn kohdan tiedostosta, voit käyttää `seek()`-funktiota ja määrittää haluamasi aloituspaikan tiedostossa. Tämä on hyödyllistä esimerkiksi jos tekstitiedostossa on tietty otsikko ja haluat aloittaa lukemisen vasta sen jälkeen.

```Arduino
// Avataan tiedosto lukemista varten
File tiedosto = SD.open("tiedosto.txt");

// Siirrytään aloituspaikkaan
tiedosto.seek(10);

// Luetaan tiedostosta
Serial.println(tiedosto.readString());
```

Tässä esimerkissä ensimmäinen rivi avaa tiedoston lukemista varten ja tallentaa sen muuttujaan `tiedosto`. Toisessa rivissä käytetään `seek()`-funktiota ja siirrytään 10 merkin päähän tiedoston alusta. Viimeisessä rivissä luetaan tiedostosta `readString()`-funktiolla.

## Syvemmälle

Tiedostojen lukeminen Arduino-koodilla vaatii yleensä SD-kortin lisäämistä ja ohjelman uudelleenkäynnistyksen jokaisen lisäyksen jälkeen. Tämä johtuu siitä, että ohjelma tarvitsee syklin uudelleenkäynnistävän `setup()`-funktion suorittamisen ennen kuin se pystyy lukemaan tiedostoja.

Jos haluat lukea tekstitiedostoja ilman SD-korttia, voit käyttää `Serial`-tulostustoimintoa Arduino IDE:ssa. Tällöin voit ohjata tiedoston lukuprosessin suoraan sarjaportin kautta.

```Arduino
// Luetaan tiedostoa sarjaportin kautta
while(Serial.available()) {
  char data = Serial.read();
  Serial.write(data);
}
```

Tässä esimerkissä ohjelma lukee tiedostoa sarjaportin kautta ja kirjoittaa sen takaisin sarjaporttiin. Tätä menetelmää voi käyttää myös tiedoston lukemiseen muista laitteista, kuten tietokoneesta tai matkapuhelimesta.

## Katso myös

- https://www.arduino.cc/en/Reference/File
- https://www.arduino.cc/reference/en/language/functions/files-io/open/
- https://create.arduino.cc/projecthub/Arduino_Genuino/sd-card-module-with-arduino-how-to-read-write-with-i-o-4e9264