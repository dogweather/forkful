---
title:                "C: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen kirjoittaminen on tärkeä osa C-ohjelmointia, sillä se mahdollistaa tiedon tallentamisen ja jakamisen eri ohjelmien välillä. Näin ollen se on välttämätöntä monimutkaisempien ohjelmien luomisessa ja datan hallinnassa.

## Miten

Tekstitiedoston kirjoittaminen C-kielellä on suhteellisen yksinkertaista. Ensiksi avataan tiedosto käyttäen `fopen()` funktiota ja määritellään sille kirjoitusmuoto. Tämän jälkeen käytetään `fprintf()` funktiota kirjoittamaan halutut tiedot tiedostoon ja lopuksi tiedosto suljetaan `fclose()` funktiolla. Alla on esimerkki:

```C
#include <stdio.h>

int main() {
    FILE *tiedosto;
    tiedosto = fopen("teksti.txt", "w"); //Avataan tiedosto kirjoitusmuodossa
    fprintf(tiedosto, "Hei, tämä on teksti tiedostossa\n"); //Kirjoitetaan teksti tiedostoon
    fclose(tiedosto); //Suljetaan tiedosto
    return 0;
}
```

Tämän koodin suorittamisen jälkeen ohjelma luo `teksti.txt` tiedoston, jossa on kirjoitettu "Hei, tämä on teksti tiedostossa". On kuitenkin tärkeää huomata, että jos tiedosto on jo olemassa, tämä koodi kirjoittaa sen päälle. Mikäli haluat lisätä tekstiä olemassa olevaan tiedostoon, voit käyttää `a` sijasta `w` määrityksessä, mikä avaa tiedoston lisäysmuodossa.

## Syväsukellus

Tekstitiedoston kirjoittaminen voidaan tehdä myös hieman monimutkaisemmalla tavalla hyödyntämällä `fprintf()` funktion muotoiluja. Tämän avulla voit muun muassa määrittää tietyn tiedon tulostamisen tietyn kohdan tiedostoon. Alla on esimerkki:

```C
#include <stdio.h>

int main() {
    FILE *tiedosto;
    int i = 5;
    float f = 3.14;
    tiedosto = fopen("teksti.txt", "w");
    fprintf(tiedosto, "Kokonaisluku: %d, Desimaaliluku: %f", i, f);
    fclose(tiedosto);
    return 0;
}
```

Tämän koodin suorittamisen jälkeen tiedostoon tulostuu "Kokonaisluku: 5, Desimaaliluku: 3.14". Voit myös määrittää muotoilun eri paikoissa tiedostossa käyttämällä `%m$n` muotoiluja, jossa `m` tarkoittaa tietojen määrää ja `n` kohdan numeroa. Esimerkiksi `%1$d` tarkoittaa ensimmäistä kokonaislukua tulostettavien tietojen joukossa. Tämä voi olla hyödyllistä esimerkiksi CSV-tiedostojen luomisessa.

## Katso myös

- C-tiedoston käsittely: https://users.cs.cf.ac.uk/Dave.Marshall/C/node22.html
- C stdio.h kirjasto: https://www.tutorialspoint.com/c_standard_library/stdio_h.htm
- Esimerkkejä tiedostojen kirjoittamisesta C-kielellä: https://www.programiz.com/c-programming/c-file-input-output