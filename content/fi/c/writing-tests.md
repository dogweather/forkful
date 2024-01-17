---
title:                "Ohjelmointitestejä kirjoittaminen"
html_title:           "C: Ohjelmointitestejä kirjoittaminen"
simple_title:         "Ohjelmointitestejä kirjoittaminen"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Testien kirjoittaminen on kun ohjelmistokehittäjä luo koodia testatakseen ohjelmansa toimintaa. Testien avulla voidaan varmistua siitä, että koodi toimii odotetulla tavalla ja estetään mahdollisten virheiden syntymistä.

## Kuinka tehdä:
Seuraavassa on esimerkki tapa luoda testi C-kielellä:
```C
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int result = add(5, 3);
    if(result == 8) {
        printf("Testi läpäisty!");
    }
    else {
        printf("Testi epäonnistui!");
    }
    return 0;
}
```
Tässä esimerkissä luodaan yksinkertainen testi funktiolle add, joka laskee kahden luvun summan. Testissä tarkistetaan, että add-funktio palauttaa odotetun tuloksen. Näin testi varmistaa, että funktio toimii oikein.

## Syvempi sukellus:
Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä, sillä se auttaa varmistamaan koodin laadun ja vähentämään virheiden määrää. Nykypäivänä on myös olemassa erilaisia testauksen työkaluja, kuten järjestelmätestaus ja yksikkötestaus, jotka auttavat kehittäjää testaamaan ohjelmansa toimintaa. Testien kirjoittaminen on myös osa hyvää ohjelmointikäytäntöä ja auttaa kehittäjää ymmärtämään paremmin omaa koodiaan.

## Katso myös:
- [Wikipedia-artikkeli testaamisesta](https://fi.wikipedia.org/wiki/Testaaminen)
- [An introduction to testdriven development in C](https://developer.ibm.com/technologies/systems/articles/au-cintrotesting/)
- [Testaamisen perusteet ohjelmistokehittäjille](https://www.sqav.fi/fi/testaamisen-perusteet-ohjelmistokehittajille/)