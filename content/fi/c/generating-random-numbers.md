---
title:                "C: Sattumanvaraisten numeroiden luominen"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet ohjelmointimaailmassa uusi, saatat ihmetellä, miksi kukaan haluaisi luoda satunnaisia numeroita C-ohjelmoinnissa. Satunnaislukugeneraattori on itse asiassa tärkeä osa monia ohjelmia ja pelejä, jotka perustuvat satunnaisuuteen. Esimerkkejä tällaisista sovelluksista ovat lottopelit, peliautomaatit, simulaatiot ja salausmenetelmät.

## Miten

C-kielellä satunnaislukujen luominen tapahtuu käyttämällä <stdlib.h> kirjastoa ja sen funktiota rand (). Tämä funktio tuottaa luvun väliltä 0 ja RAND_MAX - 1, joka voi vaihdella järjestelmän mukaan. Oikean satunnaisuuden varmistamiseksi on tärkeää kutsua funktiota srand () ensin, joka asettaa satunnaislukugeneraattorin alkutilaan.

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // asetetaan satunnaislukugeneraattorin alkutila
    srand(time(0));

    // tuotetaan satunnainen luku väliltä 0 ja 99
    int random = rand() % 100;

    // tulostetaan satunnainen luku
    printf("Satunnainen luku väliltä 0 ja 99: %d \n", random);
    
    return 0;
}
```

Esimerkkitulostus:

```
Satunnainen luku väliltä 0 ja 99: 73
```

## Syvällisempi tarkastelu

Satunnaislukugeneraattorit perustuvat usein matemaattisiin kaavoihin tai fyysisiin prosesseihin, kuten näytteenottoon ilmavirtauksista tai radiolähteistä. Ne pyrkivät tuottamaan lukuja, jotka ovat mahdollisimman satunnaisia ja toistumattomia. On kuitenkin tärkeää huomata, että luvut eivät ole täysin satunnaisia ja ennalta arvaamattomia, vaan niiden arvot perustuvat laskentamenetelmiin, jotka käsittelevät tietokoneella saatavilla olevia tietoja.

On myös tärkeää huomata, että satunnaislukugeneraattorit eivät sovellu kryptografisiin tarkoituksiin, joissa tarvitaan erittäin satunnaista ja ennustamatonta lukujonoa. Tällaisiin tarkoituksiin tulisi käyttää kryptografista satunnaislukugeneraattoria.

## Katso myös

- [Satunnainen luku C-ohjelmassa](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Satunnaislukugeneraattorien toimintaperiaatteet](https://en.wikipedia.org/wiki/Random_number_generation)
- [Cryptographically secure random number generator in C](https://gist.github.com/rdb/8864666)