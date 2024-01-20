---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Satunnaislukujen generointi on prosessi, jossa saadaan aikaan ennustamaton numerojono. Ohjelmoijat käyttävät sitä esimerkiksi simulaatioissa, salausalgoritmeissa ja pelilogiikassa, joka vaatii sattumanvaraisuutta.

## Näin se tehdään:

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd;  // luo satunnaislukulaite
    std::mt19937 gen(rd()); // alusta generointi Mersenne Twister -algoritmilla
    std::uniform_int_distribution<> dis(1, 6); // määrittää jakelun välillä 1 ja 6

    for (int n=0; n<10; ++n)
        std::cout << dis(gen) << " "; // tulosta 10 sattumanvaraista lukua
}
```
Tätä koodia suoritettaessa saadaan tulostukseksi esimerkiksi seuraava sekvenssi: `4 1 6 3 5 2 2 4 6 3`

## Syvemmälle meneminen:

### Historiallinen konteksti
Historiallisesti satunnaislukuja on käytetty erilaisissa tilanteissa, joissa pitää tehdä sattumanvarainen valinta tai arvioida tietyn ilmiön todennäköisyys. Tietokoneohjelmissa usein käytetty vanhempi menetelmä on rand()-funktio, kuitenkin uudemmissa C++ -standardeissa suositellaan käyttämään yllä esiteltyä menetelmää.

### Vaihtoehtoja
On myös muita menetelmiä, kuten lineaariset kongruenssialgoritmit ja Lagged Fibonacci Generator. Jokaisella on omat vahvuudet ja heikkoudet, ja valinta riippuu sovelluksen vaatimuksista.

### Toteutuksen yksityiskohdat
Edellä määritellyssä koodissa "random_device" on laitteistopohjainen satunnaislukugeneraattori, jos sellainen on saatavilla. "mt19937" on Mersenne Twister -algoritmi, joka on yksi tunnetuimmista satunnaislukugeneraattoralgoritmeista.

## Katso myös:

- C++ kirjasto dokumentaatio: [www.cplusplus.com/reference/random/](http://www.cplusplus.com/reference/random/)
- Erilaisia satunnaislukugeneraattoreita: [en.wikipedia.org/wiki/List_of_random_number_generators](https://en.wikipedia.org/wiki/List_of_random_number_generators)