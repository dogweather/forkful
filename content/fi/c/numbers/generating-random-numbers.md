---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:47.675787-07:00
description: "Satunnaislukujen generointi C-kieless\xE4 tarkoittaa arvojen luomista,\
  \ jotka ovat ennalta arvaamattomia ja noudattavat tietty\xE4 jakaumaa, kuten tasajakaumaa\u2026"
lastmod: '2024-03-13T22:44:57.035202-06:00'
model: gpt-4-0125-preview
summary: "Satunnaislukujen generointi C-kieless\xE4 tarkoittaa arvojen luomista, jotka\
  \ ovat ennalta arvaamattomia ja noudattavat tietty\xE4 jakaumaa, kuten tasajakaumaa\
  \ tai normaalijakaumaa."
title: Satunnaisten numeroiden generointi
weight: 12
---

## Kuinka:
C-kielessä satunnaislukuja voidaan generoida käyttämällä `rand()`-funktiota, joka on osa C-standardikirjastoa `<stdlib.h>`. Oletuksena `rand()` tuottaa pseudo-satunnaislukuja väliltä 0 `RAND_MAX` (vakio, joka on määritelty `<stdlib.h>`-kirjastossa). Tarkemmalle kontrollille alueesta ohjelmoijat voivat manipuloida `rand()`-funktion tulostetta.

Tässä on yksinkertainen esimerkki satunnaisluvun generoinnista väliltä 0–99:

```c
#include <stdio.h>
#include <stdlib.h> // rand() ja srand() varten
#include <time.h>   // time() varten

int main() {
    // Kylvä satunnaislukugeneraattori
    srand((unsigned) time(NULL));

    // Generoi satunnaisluku väliltä 0–99
    int randomNumber = rand() % 100;

    printf("Satunnaisluku: %d\n", randomNumber);

    return 0;
}
```

Esimerkkituloste voi vaihdella joka kerta, kun ohjelma suoritetaan:

```
Satunnaisluku: 42
```
Satunnaislukujen generoimiseksi eri alueelta, voi modulo-operaattoria (`%`) säätää tarpeen mukaan. Esimerkiksi `rand() % 10` generoi lukuja väliltä 0–9.

On tärkeää huomata, että pseudo-satunnaislukugeneraattorin kylvö (`srand()`-kutsu) nykyisellä ajalla (`time(NULL)`) varmistaa erilaiset satunnaislukusarjat ohjelman suoritusten välillä. Ilman kylvöä (`srand()`) `rand()` tuottaisi saman sarjan numeroita joka kerta, kun ohjelma suoritetaan.

## Syväsukellus
`rand()`-funktio ja sen kylvöpari `srand()` ovat olleet osa C-standardikirjastoa vuosikymmeniä. Ne perustuvat algoritmeihin, jotka generoivat numerojonoja, jotka vain vaikuttavat satunnaisilta—siitä termi "pseudo-satunnainen." `rand()`-funktion taustalla oleva algoritmi on tyypillisesti lineaarinen kongruentiaaligeneraattori (LCG).

Vaikka `rand()` ja `srand()` ovat riittäviä monille sovelluksille, niiden tiedetään olevan rajoittuneita, erityisesti satunnaisuuden laadun ja mahdollisen ennustettavuuden suhteen. Sovelluksille, jotka vaativat korkealaatuista satunnaisuutta, kuten kryptografiset toimet, tulee harkita vaihtoehtoja, kuten `/dev/random` tai `/dev/urandom` (Unix-tyyppisissä järjestelmissä), tai kryptografisten kirjastojen tarjoamia API:ja.

C11:n esittelyssä, ISO C -standardiin lisättiin uusi otsikkotiedosto, `<stdatomic.h>`, joka tarjoaa hienosyisemmän kontrollin rinnakkaistoiminnoille, mutta ei suoraan liity satunnaisuuteen. Todellisen satunnaisuuden saavuttamiseksi C:ssä, kehittäjät kääntyvät usein alusta-spesifisten tai ulkoisten kirjastojen puoleen, jotka tarjoavat parempia algoritmeja tai hyödyntävät laitteistopohjaisia entropialähteitä.

Muista, että vaikka `rand()` toimii yksinkertaisena ja saavutettavissa olevana keinona generoida pseudo-satunnaislukuja, sen käyttöalue modernissa sovelluskehityksessä on rajoittunut tulosteen laadun ja ennustettavuuden vuoksi. Kun tarvitaan vankempia ratkaisuja, erityisesti turvallisuustietoisissa sovelluksissa, standardikirjaston ulkopuolelle tutkiminen on erittäin suositeltavaa.
