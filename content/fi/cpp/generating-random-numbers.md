---
title:                "C++: Satunnaisten lukujen luominen"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Monilla ohjelmoijilla voi olla tarve generoida satunnaisia numeroita erilaisten sovellusten ja tehtävien parissa. Satunnaiset numerot voivat auttaa esimerkiksi simuloimaan tilanteita, laatimaan testidataa tai tarjoamaan vaihtelua pelissä.

## Kuinka

Satunnaisien numeroiden generointi onnistuu helposti C++:lla käyttämällä funktiota rand() ja siihen liittyvää siemensize seed. Ensiksi täytyy lisätä oma siemensize, esimerkiksi ajan mukaan, jotta satunnaisluvut vaihtuvat jokaisella ohjelman käynnistyksellä.

```
#include <iostream>
#include <cstdlib> // rand()
#include <time.h> // time()

int main()
{
    srand(time(NULL)); // lisätään siemensize

    // generoidaan satunnainen numero väliltä 1-10
    int satunnainenLuku = rand()%10 + 1;
    std::cout << "Satunnainen luku väliltä 1-10: " << satunnainenLuku << std::endl;
    return 0;
}
```

Tässä esimerkissä käytetään modulo-operaatiota, jotta luvut pysyvät halutulla välillä. Voit muuttaa välit haluamaksesi, esimerkiksi jos haluat satunnaisia desimaalilukuja, voit muuttaa koodia ja sen tulostusta vastaavasti.

## Syvempi sukellus

Edellä mainitun esimerkin lisäksi C++:lla on muitakin tapoja generoida satunnaisia lukuja, kuten esimerkiksi funktiot srand48() ja drand48(), jotka ovat tarkoitettu käytettäväksi desimaalilukujen kanssa. On myös huomioitava, että funktio rand() ei ole täysin satunnainen ja sen luvut voivat toistua pitkässä juoksussa. Jos tarvitset täysin satunnaisia lukuja, kannattaa tutustua esimerkiksi Boost-kirjaston random-toimintoihin.

## Katso myös

- [C++ dokumentaatio satunnaisille luvuille](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Boost-kirjaston random-toiminnot](https://www.boost.org/doc/libs/1_77_0/doc/html/random.html)