---
title:    "C++: Satunnaislukujen generoiminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi 

Tervehdys kaikille! Oletko koskaan miettinyt, miksi C++ -ohjelmointikielellä tehdään satunnainen numeroiden generointi? Satunnaiset numerot ovat hyödyllisiä monissa ohjelmoinnin sovelluksissa, kuten peleissä ja salausalgoritmeissa. Joten älä huoli, tämä blogikirjoitus selittää kaiken tarvittavan.

## Miten

Satunnaisien numeroiden generointi C++:lla on helppoa ja nopeaa! Käytämme siihen standardikirjaston <random> ja <iostream>. Alla olevassa koodiesimerkissä käytämme satunnaisen luvun generoimiseen seed-arvoa (siemen), jotta saamme erilaisia lukuja joka kerta suorittaessamme koodin.

````C++
#include <iostream>
#include <random>

int main() {
  // Alustetaan seed-arvo
  std::random_device rd;
  // Käytetään seed-arvoa generaattorissa
  std::mt19937 mt(rd());
  // Määritetään haluttu arvojen väli
  std::uniform_int_distribution<int> dist(1, 10);

  // Tulostetaan 10 satunnainen lukua
  for (int i = 0; i < 10; i++) {
    std::cout << dist(mt) << " ";
  }

  return 0;
}
````
Tämän koodiesimerkin tulostus voi esimerkiksi olla seuraavanlainen:
5 9 6 3 10 1 4 2 7 8

## Syvemmälle

Nyt kun tiedämme, kuinka generoida satunnaisia lukuja C++:lla, haluamme ehkä tietää, miten tämä tapahtuu taustalla. Generointi perustuu pseudo-satunnaislukugeneraattoriin, joka tuottaa lukuja tietyllä kaavalla seed-arvon perusteella. Seed-arvo puolestaan saadaan usein järjestelmän kellosta tai ruutupiirroksen tulemisesta. Näitä seed-arvoja yhdistämällä ja käyttämällä monimutkaisempia kaavoja, voidaan tuottaa näennäisesti satunnaisia lukuja. Kuitenkin näiden lukujen todellinen satunnaisuus riippuu käytetyistä kaavoista ja antureista.

## Katso myös

- [C++:n random -kirjasto](https://en.cppreference.com/w/cpp/numeric/random)
- [Satunnaislukugeneraattorit ja pseudosatunnaisuus](https://www.youtube.com/watch?v=4vJphRN3fFQ)
- [Satunnaislukujen käyttö peleissä](https://www.gamasutra.com/view/feature/131500/random_number_generation_.php)