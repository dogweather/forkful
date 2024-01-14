---
title:    "C++: Satunnaisten numeroiden luominen"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Miksi: Satunnaisnumeroita kannattaa generoida

On monia syitä, miksi ohjelmoijat haluavat generoida satunnaisia numeroita. Näihin kuuluvat esimerkiksi simulaatiot, pelit, salaus ja tilastolliset analyysit. Satunnaislukujen käyttö voi myös auttaa luomaan ennustettavuutta ja vähentämään mahdollisia tietoturvariskejä.

## Kuinka: Satunnaisnumerojen luominen C++:lla

C++:lla on useita tapoja generoida satunnaisia numeroita. Yksi yleisimmistä tavoista on käyttää <code>rand()</code> -funktiota, joka antaa satunnaisen numeron väliltä 0 ja <code>RAND_MAX</code> (yleensä 32767) välillä. Alla on esimerkki, miten voit käyttää tätä funktiota C++:lla:

```C++
#include <iostream>
#include <cstdlib> // sisältää rand() funktion

int main() {
   int random_number = rand();
   std::cout << random_number << std::endl; // tulostaa satunnaisen numeron
   return 0;
}
```

Tämä esimerkki tulostaa aina saman satunnaisen numeron, koska emme ole asettaneet alkuarvoa <code>rand()</code> -funktiolle. Voit muuttaa satunnaisen luvun alkuarvoa alla olevan esimerkin avulla:

```C++
#include <iostream>
#include <cstdlib> // sisältää rand() funktion
#include <ctime> // sisältää ajan funktion time()

int main() {
   // alustaa ajan satunnaisen numeron alkuarvoksi
   srand(time(0));

   int random_number = rand();
   std::cout << random_number << std::endl; // tulostaa satunnaisen numeron
   return 0;
}
```

Nyt tulostettava satunnainen numero muuttuu joka kerta, kun koodi suoritetaan.

## Syvemmälle: Satunnaislukujen generoiminen

Satunnaislukujen generoiminen ei ole täysin satunnaista matematiikkaa, vaan se perustuu matemaattisiin algoritmeihin. Käytännössä nämä algoritmit luovat suuria numerojoukkoja, joista sitten valitaan satunnaisia lukuja. Mikäli algoritmi ei ole riittävän monimutkainen, satunnaisina pidettyjä lukuja voidaan ennustaa ja tietoturva on uhattuna.

Toinen tärkeä tekijä on satunnaisuuden jakautuminen. Jos satunnaiset luvut eivät jakaannu tasaisesti, ne eivät ole todellista satunnaisuutta. Tämän takia C++ tarjoaa myös muita satunnaislukujen generointifunktioita, kuten <code>rand_r()</code>, <code>srand48()</code>, ja <code>drand48()</code>, jotka tarjoavat erilaisia algoritmeja ja jakaumia.

## Katso myös

- C++ <code>rand()</code> -funktion dokumentaatio: https://www.cplusplus.com/reference/cstdlib/rand/
- Satunnaislukujen generoinnin parhaat käytännöt: https://www.geeksforgeeks.org/best-practices-for-random-number-generation-in-c/
- Satunnaislukugeneraattoreiden vertailu: https://en.wikipedia.org/wiki/Comparison_of_random_number_generators