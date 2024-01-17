---
title:                "Standard errorin kirjoittaminen"
html_title:           "C++: Standard errorin kirjoittaminen"
simple_title:         "Standard errorin kirjoittaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kirjoittaminen ns. standardivirhe-kanavaan (engl. standard error) on tapa ilmoittaa ohjelman suorituksessa tapahtuvista virheistä. Ohjelmoijat hyödyntävät tätä tekniikkaa jotta virhetilanteet voidaan havaita ja korjata nopeammin.

## Kuinka:
```C++
#include <iostream>

int main() {
  std::cerr << "Tämä on esimerkki virheilmoituksesta.\n";
  return 0;
}
```
Esimerkki tulosteesta:
```
Tämä on esimerkki virheilmoituksesta.
```

## Syvempi sukellus:
Kirjoittaminen standardivirhe-kanavaan perustuu Unix-käyttöjärjestelmässä kehitettyyn tekniikkaan, jossa tulostuksessa käytetään kolmea eri kanavaa: tavallinen tulostus (stdout), virheilmoitukset (stderr) ja käyttöohjeet (stdvin). Tämä tekniikka on myöhemmin implementoitu myös moniin muihin käyttöjärjestelmiin, kuten Windowsiin.

Yksi vaihtoehto kirjoittamiselle standardivirhe-kanavaan on käyttää virheilmoituksia käsitteleviä funktioita, kuten `perror` tai `strerror`. Näitä voidaan hyödyntää esimerkiksi tulostamaan lisätietoja virhetilanteista.

Näin voidaan kirjoittaa virheilmoitus käyttäen `perror`-funktiota:
```C++
#include <stdio.h>
#include <errno.h>

int main () {
  FILE *fp;
  fp = fopen("tiedosto.txt", "r");
  if (fp == NULL) {
    perror("Virhe avattaessa tiedostoa: ");
    return 1;
  }
  return 0;
}
```
Tämä tulostaa virheilmoituksen, johon liitetään automaattisesti selitys virheen syystä ja sijainnista.

## Katso myös:
- [Unix-kanavien perusteet](https://en.wikipedia.org/wiki/Standard_streams)
- [C++ virheiden käsittely](https://www.tutorialspoint.com/cplusplus/cpp_exceptions_handling.htm)