---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:48:28.118025-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Satunnaislukujen generointi tarkoittaa arvaamattomien numeroiden tuottamista. Koodaajat käyttävät niitä peleissä, simulaatioissa ja turvallisuussovelluksissa, joissa ennakoimaton käytös on olennaista.

## How to: (Kuinka Tehdään:)
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // initialise random number generator
    srand(time(NULL));

    // generate and print a random number
    printf("Random number: %d\n", rand());

    return 0;
}
```
Esimerkitulostus voisi olla:
```
Random number: 50732
```

## Deep Dive (Sukellus Syvemmälle)
C-kielessä `rand()` on klassinen tapa luoda satunnaislukuja, mutta se tuottaa ennalta määrätyn sarjan. `srand()` kutsutaan kerran ohjelmassa asettamaan satunnaislukugeneraattorin siemen, joka usein on `time(NULL)` varmistamaan ainutlaatuisuutta.

Ennen C11-standardia, satunnaisuus oli rajoittunutta ja ennustettavaa, joka ei sovi kaikkiin käyttötarkoituksiin. C11 toi mukanaan `_Generic` makron, mikä mahdollistaa paremmin satunnaislukugeneraattoreiden käytön.

Vaihtoehteja `rand()`:lle on, kuten `/dev/random` ja `/dev/urandom` Unix-järjestelmissä sekä erilaiset kryptograafiset kirjastot, jotka tarjoavat vahvemman satunnaisuuden.

## See Also (Katso Myös)
- C Standard Library documentation: https://en.cppreference.com/w/c/numeric/random
- An article on random number generation in C: https://www.learncpp.com/cpp-tutorial/59-random-number-generation/
- Information about randomness in cryptography: https://www.schneier.com/crypto-gram/archives/1999/0915.html