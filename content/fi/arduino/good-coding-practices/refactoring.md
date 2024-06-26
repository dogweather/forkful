---
date: 2024-01-26 01:17:02.262412-07:00
description: "Miten: Oletetaan, ett\xE4 sinulla on Arduino-ohjelmassasi funktio, joka\
  \ tekee aivan liikaa, kuten t\xE4ss\xE4."
lastmod: '2024-03-13T22:44:56.831970-06:00'
model: gpt-4-0125-preview
summary: "Oletetaan, ett\xE4 sinulla on Arduino-ohjelmassasi funktio, joka tekee aivan\
  \ liikaa, kuten t\xE4ss\xE4."
title: Koodin refaktorointi
weight: 19
---

## Miten:
Oletetaan, että sinulla on Arduino-ohjelmassasi funktio, joka tekee aivan liikaa, kuten tässä:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Funktio, joka tekee liikaa
  handleEverything();
}

void handleEverything() {
  // Lue anturidata
  int sensorValue = analogRead(A0);
  // Käsittele anturidata
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // Tulosta anturidata
  Serial.println(sensorValue);
  delay(500);
}
```

Refaktoroinnin jälkeen se saattaisi näyttää tältä, kun `handleEverything()`-funktio jaetaan pienempiin, tarkemmin keskittyviin funktioihin:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

Refaktoroinnin jälkeen `loop()`-funktio on helpommin luettava ja jokainen tehtävä käsitellään omalla funktiollaan, tehden koodista helpommin hallittavaa.

## Syväsukellus
Historiallisesti refaktorointi on tullut suosituksi Agile- ja Test-Driven Development (TDD) metodologioiden myötä, jotka perustuvat jatkuvaan koodin parantamiseen mukautuakseen muuttuviin vaatimuksiin. Refaktoroinnissa on useita työkaluja ja strategioita — kuten "Extract Method" -tekniikka, jota käytimme Arduino-esimerkissämme. Tämä on olennaista, kun siirrytään nopeasta prototyypistä vakaaseen projektiin, jossa koodin luettavuus ja ylläpitäminen muuttuvat keskeisiksi.

Refaktoroidessa on tärkeää, että käytössä on hyvä testien joukko varmistamaan, että muutokset eivät ole tuoneet mukanaan bugeja. Arduino-maailmassa automatisoidut testit eivät aina ole suoraviivaisia laitteistoriippuvuuksien vuoksi, mutta voit silti käyttää yksikkötestausta puhtaille loogisille osille tai käyttää simulaattoreita.

Vaihtoehtoja manuaaliselle refaktoroinnille ovat erikoistuneet refaktorointityökalut, jotka automatisoivat koodihajujen tunnistamisen ja ehdottavat muutoksia. Nämä työkalut kuitenkin usein puuttuvat mikrokontrollerikoodin hienovaraisuuksista ja eivät välttämättä ole saatavilla Arduino-kehitysympäristössä.

Lopulta refaktorointi on taidetta, joka tasapainottaa koodin sisäisen rakenteen parantamista virheiden tuomisen riskiä vastaan. Se vaatii sinua pohtimaan toteutuksen yksityiskohtia, kuten muistin käyttöä ja prosessoriaikaa, erityisesti resurssien rajoittaman mikrokontrollerien luonteen vuoksi.

## Katso myös
Voit sukeltaa syvemmälle refaktorointiin Martin Fowlerin uraauurtavan kirjan *Refaktorointi: Olemassa olevan koodin suunnittelun parantaminen* avulla. Arduinon erityiskäytäntöihin voit tutustua tarkemmin Arduino-kehitysfoorumeilla ja yhteisöissä:

- [Arduino Forum - Programming Questions](https://forum.arduino.cc/index.php?board=4.0)
- [Refactoring Guru](https://refactoring.guru/refactoring)

Muista, että tavoitteena on puhdas, ymmärrettävä koodi, josta sekä tuleva sinä että muut kiittävät sinua. Jatka hakkerointia ja pidä se siistinä!
