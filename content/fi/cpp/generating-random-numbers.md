---
title:                "Satunnaislukujen generointi"
aliases:
- fi/cpp/generating-random-numbers.md
date:                  2024-01-27T20:32:46.578452-07:00
model:                 gpt-4-0125-preview
simple_title:         "Satunnaislukujen generointi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Satunnaislukujen tuottaminen ohjelmoinnissa tarkoittaa numerosekvenssien luomista, joissa ei ole ennustettavaa järjestystä tai kaavaa. Ohjelmoijat käyttävät usein näitä numeroita erilaisiin tarkoituksiin, kuten arvaamattomien tapahtumien simulointiin, testauksessa ja virheenkorjauksessa sekä pelialgoritmeissä oikeudenmukaisuuden tai arvaamattomuuden varmistamiseksi.

## Kuinka:

Satunnaislukujen generoimiseksi C++:ssa tyypillisesti käytetään `<random>` otsikkotiedostoa, joka esiteltiin C++11:ssä, tarjoten laajan valikoiman välineitä satunnaislukujen generoimiseen eri jakautumista.

```C++
#include <iostream>
#include <random>

int main() {
    // Alusta satunnaislukugeneraattori
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Määritä väli [0, 99] mukaan lukien
    std::uniform_int_distribution<> distrib(0, 99); 

    // Generoi ja tulosta 5 satunnaista lukua määritellyltä väliltä
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Tämä koodiesimerkki alustaa Mersenne Twister -satunnaislukugeneraattorin siemenarvolla, joka on otettu `std::random_device`:stä. Sitten se määrittelee yhtenäisen kokonaislukujakautuman välille [0, 99] ja lopuksi tulostaa 5 satunnaista numeroa tästä jakautumasta.

Esimerkkitulostus saattaa näyttää tältä, mutta pidä mielessä, että jokainen suoritus tuottaa todennäköisesti erilaisia tuloksia:

```
45 67 32 23 88
```

## Syväsukellus:

Historiallisesti satunnaislukujen generointi C++:ssa nojasi vahvasti `rand()`-funktioon ja siemenarvon asettamiseen `srand()`-funktiolla, jotka löytyvät `<cstdlib>` otsikkotiedostosta. Tämä lähestymistapa sai kuitenkin usein kritiikkiä sen puutteellisen yhtenäisyyden ja ennustettavuuden vuoksi generoitujen numeroiden jakautumassa.

`<random>` otsikkotiedoston esittely C++11:ssä merkitsi merkittävää parannusta, tarjoten kehittyneen järjestelmän satunnaisten lukujen tuottamiseen. Tarjotut välineet sisältävät valikoiman moottoreita (kuten `std::mt19937` Mersenne Twisterille) ja jakautumia (kuten `std::uniform_int_distribution` yhtenäiselle kokonaislukujen jakautumalle), joita voidaan yhdistellä ohjelmoijan erityistarpeiden mukaan, johtaen ennustettavampaan käytökseen, parempaan suorituskykyyn ja suurempaan joustavuuteen.

Vaikka `<random>` kirjasto on paljon parempi kuin vanhempi `rand()`-lähestymistapa, on syytä huomata, että todella satunnaisten lukujen generointi — erityisesti kryptografisiin tarkoituksiin — riippuu edelleen lisäharkinnoista. Kryptografisiin sovelluksiin tarkoitetut kirjastot, jotka usein hyödyntävät laitteistopohjaisia entropialähteitä, tulisi käyttää sen sijaan.
