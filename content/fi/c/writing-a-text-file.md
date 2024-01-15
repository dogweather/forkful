---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "C: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Kirjoittaminen on yksi tärkeimmistä ohjelmoinnin osa-alueista, ja tiedostojen käsittely on olennainen osa tätä taitoa. Tekstitiedostojen kirjoittaminen antaa sinulle mahdollisuuden tallentaa tietoja pysyvästi ja jakaa niitä muiden kanssa. Se on myös tärkeää, jos haluat luoda ohjelmia, jotka käyttävät syötettä ja tulostetta.

## Miten
Tekstitiedoston kirjoittaminen C:n avulla on helppoa. Ensimmäinen asia mitä sinun tulee tehdä on avata tiedostokahva käyttäen fopen-funktiota ja määrittelemällä tiedoston nimi ja haluamasi kirjoitusmuoto. Esimerkiksi:
```C
FILE *tiedosto = fopen("tekstitiedosto.txt", "w");
```
Tämän jälkeen voit käyttää fprintf-funktiota kirjoittamaan haluamasi tekstitiedoston sisältö. Esimerkiksi:
```C
fprintf(tiedosto, "Tämä on tekstiä, jota kirjoitetaan tekstitiedostoon.");
```
Kun olet lopettanut kirjoittamisen, muista sulkea tiedostokahva käyttäen fclose-funktiota.
```C
fclose(tiedosto);
```
Tämän jälkeen voit lukea tekstitiedoston käyttäen esimerkiksi scanf-funktiota.

## Deep Dive
Tekstitiedostojen kirjoittaminen tarjoaa mahdollisuuden käyttää monia erilaisia C:n tiedostokirjoitusfunktioita, kuten fprintf, fputs ja fwrite. Jokaisella näistä on omat ominaisuutensa ja käyttötapansa. Voit myös käyttää C:n merkkijonofunktiota, kuten fputs ja puts, kirjoittaaksesi tekstitiedostoon. On tärkeää huomata, että tekstitiedostojen kirjoittamisessa tulee aina huolehtia tiedoston sulkemisesta, jotta vältetään mahdolliset ongelmat tiedostojen käsittelyssä.

## Katso myös 
- [C-tiedostokäsittelyopas](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C-fprintf opetusohjelma](https://www.codingunit.com/c-tutorial-fprintf-function)
- [Merkkijonofunktiot C:ssä](https://www.tutorialspoint.com/cprogramming/c_strings.htm)