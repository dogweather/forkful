---
title:                "Sattumanvaraisten numeroiden luominen"
html_title:           "C++: Sattumanvaraisten numeroiden luominen"
simple_title:         "Sattumanvaraisten numeroiden luominen"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi käyttäjä haluaisi generoida satunnaisia lukuja C++:lla. Satunnaiset numerot ovat hyödyllisiä esimerkiksi pelisuunnittelussa, satunnaisten järjestysten luomisessa ja testaamisessa.

## Kuinka tehdä se

Käytä standardikirjaston `<cstdlib>` kirjastoa, joka sisältää funktion `rand()`, joka generoi satunnaisia lukuja. Ensin tulee kutsua `srand()` funktiota, joka asettaa satunnaislukugeneraattorin siemenen. Sen jälkeen voidaan kutsua `rand()` funktiota ja tulostaa sen palauttamat luvut halutussa muodossa.

```C++
#include <cstdlib>
#include <iostream>

int main() {
  // Asettaa satunnaislukugeneraattorin siemenen.
  // Parametriksi voidaan antaa esimerkiksi ajanhetki, jotta saataisiin uusia satunnaislukuja joka suorituksella. 
  srand(time(0));

  // Generoidaan 10 satunnaista kokonaislukua väliltä 1-100 ja tulostetaan ne.
  for(int i = 0; i < 10; i++) {
    int random = rand() % 100 + 1; // generoi satunnaisen luvun väliltä 0-99 ja lisää 1 välttääkseen nollan
    std::cout << random << std::endl;
  }

  return 0;
}
```

Ohjelman tulostus voisi näyttää esimerkiksi tältä:

```
43
72
16
87
5
55
19
94
32
66
```

## Syvempi sukellus

Satunnaislukugeneraattorit käyttävät algoritmeja generoidakseen "satunnaisen" luvun. Koska koneet eivät pysty tuottamaan täysin satunnaisia lukuja, voidaan vain puhua pseudosatunnaisista luvuista, joiden arvot määräytyvät annettujen algoritmejen perusteella. Siksi tärkeää onkin valita hyvä satunnaislukugeneraattori, joka tuottaa lukuja mahdollisimman tasaisesti ja suoraviivaisesti.

Hyvä tapa parantaa satunnaislukujen laatua on käyttää parempaa satunnaislukugeneraattoria, kuten `mt19937` tai `minstd_rand`. Nämä algoritmit pohjautuvat parempiin laskentakaavoihin ja tuottavat näin ollen "satunnaisempia" lukuja.

## Katso myös

- [C++ Reference - rand()](https://www.cplusplus.com/reference/cstdlib/rand/)
- [C++ Reference - srand()](https://www.cplusplus.com/reference/cstdlib/srand/)
- [GeeksforGeeks - Random Numbers in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)