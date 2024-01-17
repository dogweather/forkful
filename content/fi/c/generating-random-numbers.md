---
title:                "Satunnaislukujen tuottaminen"
html_title:           "C: Satunnaislukujen tuottaminen"
simple_title:         "Satunnaislukujen tuottaminen"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen generointi tarkoittaa satunnaisten numeroiden luomista ohjelmallisesti. Ohjelmoijat käyttävät tätä toimintoa monissa eri tilanteissa, kuten pelin arvontaominaisuuksissa tai tietokannan käsittelyssä.

## Kuinka:
```
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Alusta satunnaislukujen generaattori
    srand(time(0));
    // Luo satunnainen luku väliltä 0-9
    int random_number = rand() % 10;
    // Tulosta luku konsoliin
    printf("Satunnainen luku: %d", random_number);
    
    return 0;
}
```

Tuloste:
```
Satunnainen luku: 7
```

## Syvennä:
Satunnaislukujen generaattori on ollut osa C-kielen standardia jo pitkään ja siitä on tullut yksi ohjelmoinnin perusominaisuuksista. C:ssä satunnaislukujen generointi tapahtuu käyttämällä `rand()` funktiota ja `srand()` funktiota, joka alustaa generaattorin käyttäen siihen syötteenä aikaa. Tämä tarkoittaa, että samaa satunnaislukua ei luoda joka kerta kun ohjelma ajetaan.

Alustava satunnaisluku voi olla joko todellisuudessa satunnainen tai lähes satunnainen. Todellisen satunnaisuuden saavuttamiseksi tarvitaan erityisiä laitteita, kuten satunnaislukugeneraattoreita. On myös muita löyhästi satunnaisia vaihtoehtoja, kuten pseudosatunnaislukugeneraattorit ja kryptografiset generaattorit.

## Katso myös:
- [rand() function (C) - GeeksforGeeks](https://www.geeksforgeeks.org/rand-function-in-c/)
- [srand() function (C) - GeeksforGeeks](https://www.geeksforgeeks.org/srand-in-ccpp/)
- [Random number generation - Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation)