---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases:
- /fi/arduino/checking-if-a-directory-exists/
date:                  2024-02-03T19:06:55.115619-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Arduino-ohjelmoinnin kontekstissa kansion olemassaolon tarkistus SD-kortilla tai vastaavassa tallennusmoduulissa mahdollistaa tiedostojen lukemisen tai kirjoittamisen ilman virheitä. Tämä toimenpide on olennainen datan kirjaamiselle, kokoonpanon hallinnalle tai mille tahansa tehtävälle, joka vaatii jäsenneltyä tiedostojen tallennusta, varmistaen luotettavuuden ja sujuvan suorituskyvyn sovelluksissasi.

## Kuinka:
Arduino ei natiivisti tue monimutkaisia tiedostojärjestelmäoperaatioita suoraan paketista. Kuitenkin käyttämällä SD-kirjastoa, joka on osa standardia Arduino IDE:tä, voit helposti työskennellä tiedostojen ja hakemistojen kanssa. Kansion olemassaolon tarkistamiseksi sinun on ensin alustettava SD-kortti ja sen jälkeen käytettävä SD-kirjaston `exists()`-metodia.

Ensiksi, sisällytä SD-kirjasto ja määritä piirin valintapinni:

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // Piirin valintapinni SD-korttimoduulille
```

`setup()`-funktiossasi, alusta SD-kortti ja tarkista onko hakemisto olemassa:

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Alustus epäonnistui!");
    return;
  }

  // Tarkista onko hakemisto olemassa
  if (SD.exists("/myDir")) {
    Serial.println("Hakemisto on olemassa.");
  } else {
    Serial.println("Hakemistoa ei ole olemassa.");
  }
}
```
`loop()`-funktiossa, voit pitää sen tyhjänä tai lisätä muita operatiivisia koodeja tarpeen mukaan:

```cpp
void loop() {
  // Operatiivinen koodi tai pidetään tyhjänä
}
```

Esimerkkituloste koodin suorittamisen jälkeen olisi joko:

```
Hakemisto on olemassa.
```
tai

```
Hakemistoa ei ole olemassa.
```

On tärkeää varmistaa, että SD-kortti on alustettu oikein ja että `/myDir` hakemistopolku vastaa erityistarpeitasi. Tämä perustarkistus on kulmakivi monimutkaisempien operaatioiden suorittamiselle tiedostoilla ja hakemistoilla SD-korteilla Ardunossa.
