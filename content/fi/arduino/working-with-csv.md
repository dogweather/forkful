---
title:                "Työskentely csv:n kanssa"
html_title:           "Arduino: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

"# Miksi ja miten CSV-tietojen käsittely Arduinolla

## Mitä & Miksi?

CSV (Comma-Separated Values) on tapa tallentaa ja järjestellä taulukkodataa käyttäen pilkkua erottimena kenttien välillä. Tämä on hyödyllistä, kun halutaan tallentaa suuria määriä dataa, jota voidaan sitten käyttää ja muokata helposti. CSV-tiedostoja käytetään laajalti esimerkiksi Excel-taulukoissa ja tietokannoissa. Koodaajat usein käsittelevät CSV-tiedostoja, koska niiden avulla tiedon tallentaminen ja järjestely on nopeaa ja helppoa.

## Miten:

Arduino:lla on valmiina kirjasto, joka mahdollistaa CSV-tiedostojen lukemisen ja kirjoittamisen. Kirjastoa käyttämällä voit ladata CSV-tiedoston toisesta lähteestä, kuten muistikortilta, ja lukea sen sisällön tai tallentaa dataa CSV-muodossa. Alla on esimerkki koodista, jossa CSV-tiedosto ladataan SD-kortilta ja data sen sisällä tulostetaan sarjaportille:

```Arduino
#include <SD.h>
#include <SPI.h>
#include <CSV.h>

File dataFile; //muuttuja tiedostoon

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; //odotetaan sarjaporttiakin käynnistyvän
  }

  if (!CSV.begin(&dataFile, "testi.csv", ";")) { //muuttuja, tiedoston nimi, erottimen merkki
    Serial.println("CSV tiedostoa ei voida avata");
  }
}

void loop() {
  if (CSV.next()) { // siirry seuraavaan riville
    int val1 = CSV.getInt(0); // ensimmäinen sarake
    float val2 = CSV.getFloat(1); // toinen sarake
    Serial.print(val1);
    Serial.print(", ");
    Serial.println(val2); // tulostaa molemmat sarakeen arvot
  } else {
    Serial.println("CSV tiedosto loppu");
    while (true); // lopeta loop
  }
}
```

Output:

```
1, 1.23
2, 4.56
3, 7.89
```

## Syvällisempi sukellus:

CSV-tiedostoilla on pitkä historia, ja niitä käytetään edelleen laajasti erilaisissa sovelluksissa. Toisinaan CSV-tiedostojen sijasta käytetään myös XML- tai JSON-formaatteja datan tallentamiseen ja järjestelyyn. CSV-tiedostojen hyvä puoli on niiden helppokäyttöisyys ja keveys, mutta ne eivät tue monimutkaisempia rakenteita kuten XML tai JSON. CSV-tiedostojen käsittelyllä on myös riski tietojen virheellisestä järjestelystä, mikäli tiedostossa ei ole selkeää rakennetta.

CSV-kirjasto Arduino:lle on vielä kehitysvaiheessa, joten tulevaisuudessa siihen saatetaan lisätä uusia ominaisuuksia ja korjauksia havaittuihin ongelmiin.

## Katso myös:

- [CSV file format explanation](https://en.wikipedia.org/wiki/Comma-separated_values)
- [Alternatives to CSV for data storage and manipulation](https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats)