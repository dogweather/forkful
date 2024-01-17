---
title:                "Sattumanvaraisten numeroiden tuottaminen"
html_title:           "C++: Sattumanvaraisten numeroiden tuottaminen"
simple_title:         "Sattumanvaraisten numeroiden tuottaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaisten numeroiden generointi on tärkeä osa ohjelmointia, jossa luodaan numeroita satunnaisesti tai tiettyjen sääntöjen mukaisesti. Ohjelmoijat tekevät tätä useista syistä, kuten pelien satunnaisuuden luomiseen, salausavaimien luomiseen tai simulointien tekemiseen.

## Miten:
Esimerkiksi, jos haluat luoda satunnaisen kokonaisluvun välillä 1-10, voit käyttää rand() toimintoa. Katso alla oleva esimerkki:

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>

using namespace std;

int main() {

    // alusta satunnaislukujen generoijat
    srand(time(0));

    // generoi ja tulosta satunnainen kokonaisluku välillä 1-10
    int random = rand() % 10 + 1;
    cout << random;

    return 0;
}
```

Tuloste voi näyttää esimerkiksi seuraavalta: `7`

## Syvemmälle:
Historiallisesti, satunnaisnumeroita on generoitu fyysisillä laitteilla, kuten nopilla tai arpakuutioilla. Nykyään käytetään usein pseudosatunnaislukugeneraattoreita, jotka perustuvat algoritmeihin ja aloituskohdan arvoon, jota kutsutaan siemeneksi. Lisäksi on olemassa muita tapoja generoida satunnaisia lukuja, kuten käyttämällä fyysisiä olosuhteita, kuten kohinan määrää tietokoneen ympärillä.

## Katso myös:
- [rand() -funktio C++:ssa (Cplusplus.com)](http://www.cplusplus.com/reference/cstdlib/rand/)
- [From Dice to Computers: A Brief History of Random Number Generation (DataGenetics)](https://datagenetics.com/blog/september22012/)
- [Introduction to Random Number Generators (Medium)](https://medium.com/@whouserandom/introduction-to-random-number-generators-dcecf0d25d34)