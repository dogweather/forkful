---
title:                "Arduino: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV-tiedostojen käsittely on tärkeä taito, joka voi olla hyödyllinen monille Arduino-ohjelmoijille. CSV-tiedostot ovat yleisiä tietomuotoja, joita käytetään useissa eri sovelluksissa, ja niiden käsittely Arduino-ympäristössä voi avata uusia mahdollisuuksia projekteissasi.

## Miten

CSV-tiedostojen käsittely Arduino-ympäristössä on helppoa käyttäen sisäistä "SD" -kirjastoa. Kirjastossa on valmiita toimintoja, jotka mahdollistavat tiedostojen lukemisen, kirjoittamisen ja muokkaamisen. Tässä on yksinkertainen esimerkki, miten voit lukea CSV-tiedoston ja tulostaa sen sisällön sarjaporttiin:

```Arduino
#include <SD.h> // lisää SD-kirjasto
File csvFile; // määritä muuttuja tiedostolle

void setup() {
    Serial.begin(9600); // alusta sarjaportti
    SD.begin(10); // alusta SD-kortin lukija
    csvFile = SD.open("data.csv"); // avaa CSV-tiedosto
}

void loop() {
  if (csvFile.available()) { // tarkista, onko tiedosto saatavilla
    String data = csvFile.readStringUntil('\n'); // lue tiedoston sisältö merkkijonoksi
    Serial.println(data); // tulosta sarjaporttiin
  }
}
```

Tässä esimerkissä käytetään "data.csv" nimistä tiedostoa, mutta voit vaihtaa sen haluamaasi tiedostonimeen.

Voit myös kirjoittaa CSV-tiedostoja Arduino-ympäristössä. Tässä on esimerkki, jossa tallennetaan sensoridataa CSV-tiedostoon ja tulostetaan tiedosto sarjaporttiin:

```Arduino
#include <SD.h>
File csvFile;
int sensorData = 0;

void setup() {
    Serial.begin(9600);
    SD.begin(10); 
    csvFile = SD.open("sensoridata.csv", FILE_WRITE);
}

void loop() {
  sensorData = analogRead(A0); // lue sensoridataa
  csvFile.print(sensorData); // tallenna data tiedostoon
  csvFile.print(','); // erottele arvot pilkulla
  delay(100); // odota 100 millisekuntia
}

void closeFile() { // lopeta tiedoston kirjoittaminen ja sulje se
  csvFile.println();
  csvFile.close();
  Serial.println("Tiedosto suljettu");
}

void serialEvent() { // odota sarjaportin viesti lopettaaksesi tiedoston kirjoittamisen
  while (Serial.available()) {
    char inChar = (char)Serial.read();
    if (inChar == 'x') { // lopetuskomento
      closeFile();
    }
  }
}
```

Tässä esimerkissä tiedosto nimetään "sensoridata.csv" ja tiedoston kirjoittaminen lopetetaan kun sarjaporttiin lähetetään viesti "x".

## Syvällinen tutustuminen

CSV-tiedostojen käsittelyn syvällisemmistä näkökulmista löytyy paljon tietoa internetistä. Yksi tärkeä asia, jonka tulisi olla tiedossa, on tietorakenteiden käyttäminen tiedostojen käsittelyssä. CSV-tiedostoja käsitellessä on tärkeää osata jakaa tiedosto oikeisiin kenttiin ja lukea niiden arvot oikeassa muodossa. Tästä on hyvä aloittaa syvempi tutustuminen tietorakenteisiin ja niiden käyttöön Arduino-ympäristössä.

## Katso myös

- [SD-kirjaston dokumentaatio](https://www.arduino.cc/en/Reference/SD)
- [CSV-tiedostojen perusteet](https://www.techwall