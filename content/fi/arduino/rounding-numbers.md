---
title:                "Numerojen pyöristäminen"
aliases:
- fi/arduino/rounding-numbers.md
date:                  2024-01-26T03:42:55.761671-07:00
model:                 gpt-4-0125-preview
simple_title:         "Numerojen pyöristäminen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/rounding-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Numeroiden pyöristäminen on desimaalin leikkaamista lähimpään kokonaislukuun tai asetettuun määrään desimaalipaikkoja. Ohjelmoijat pyöristävät numeroita, jotta niitä olisi helpompi lukea ja käsitellä, erityisesti kun tarkkuus tietyn pisteen jälkeen on tarpeetonta tai voi johtaa virheisiin.

## Kuinka:
Arduinossa voit pyöristää numeroita käyttäen sisäänrakennettuja funktioita. Keskeisiä toimijoita ovat `round`, `ceil` ja `floor`. Tässä nopea esittely:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // Pyöristää lähimpään kokonaislukuun
  Serial.println(round(myNumber)); // Tuloste: 123

  // Pyöristää aina ylöspäin
  Serial.println(ceil(myNumber));  // Tuloste: 124

  // Pyöristää aina alaspäin
  Serial.println(floor(myNumber)); // Tuloste: 123
}

void loop() {
  // Ei mitään läpikäytävää.
}
```

## Syväsukellus:
Pyöristysalgoritmeilla on pitkä historia; ne ovat olleet olemassa kauan ennen digitaalisia tietokoneita. Analogisessa laskennassa pyöristäminen oli fyysinen prosessi. Digitaalisessa laskennassa se on matemaattinen prosessi.

Pyöristäminen on tarpeen, kun muunnetaan tyyppiä, jolla on enemmän tarkkuutta (kuten `float` tai `double`), tyyppiin, jolla on vähemmän tarkkuutta (kuten `int`). Mutta tapa, jolla pyöristämme, voi vaihdella:

1. `round()`: Tavallinen pyöristys. Jos murto-osa on 0,5 tai suurempi, se pyöristetään ylöspäin; muuten alaspäin.
2. `ceil()`: Lyhenne sanasta "ceiling", pyöristää aina ylöspäin lähimpään kokonaislukuun, vaikka se olisi lähempänä alempaa numeroa.
3. `floor()`: Kattofunktion vastakohta; pyöristää aina alaspäin.

Näiden funktioiden välillä valinta riippuu siitä, mihin pyöristetty arvo on tarkoitettu. Mittaukset saattavat tarvita tavallista pyöristystä, raha usein käyttää `floor`-funktiota, kun taas varastojärjestelmät saattavat käyttää `ceil`-funktiota varmistaakseen, että kaikki on huomioitu.

Arduinon toteutus näistä funktioista on suoraviivaista; ne eivät käsittele ylimääräisiä tapauksia, kuten pyöristämistä tiettyihin desimaalipaikkoihin. Tätä varten tarvitaan joko mukautettu funktio tai syvempi matematiikka – ajattele desimaalin siirtämistä kertomalla, pyöristämistä ja sitten jakamista takaisin.

Pyöristysvirheet voivat kumuloitua, mikä vaikuttaa merkittävästi pitkiin laskelmiin tai iteratiivisiin prosesseihin. Ohjelmoijien on oltava varovaisia suorittaessaan lukuisia toimenpiteitä pyöristetyillä arvoilla.

## Katso Myös:
2. Syvällinen katsaus pyöristämisen vaaroihin ja strategioihin: [Floating Point Guide](https://floating-point-gui.de/)
3. Lisämenetelmiä, mukaan lukien mukautetut pyöristysfunktiot ja pyöristysvirheen käsittely, saatat tarkastella akateemisia resursseja tai yksityiskohtaisia ohjelmointioppaita.
