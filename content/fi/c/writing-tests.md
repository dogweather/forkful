---
title:                "C: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Koodin testaaminen on tärkeä osa ohjelmistokehitystä. Se varmistaa, että koodi toimii oikein ja vähentää virheiden mahdollisuutta tuotantoympäristössä. Kirjoittamalla testejä varmistat myös, että tulevaisuudessa tehtävät muutokset eivät riko olemassa olevaa koodia.

## Miten tehdä

Testien kirjoittaminen voi aluksi tuntua työläältä, mutta siitä on paljon hyötyä pitkällä aikavälillä. Tässä on yksinkertainen esimerkki testin kirjoittamisesta C-ohjelmalle:

```C
// Testattava funktio, joka palauttaa luvun neliön
int square(int number) {
    return number * number;
}

// Testifunktio, joka tarkistaa, että square-funktio palauttaa oikean luvun neliön
int test_square() {
    int result = square(5);
    if (result == 25) {
        printf("Test passed!");
    }
    else {
        printf("Test failed!");
    }
    return 0;
}

int main() {
    test_square();
    return 0;
}
```

Testin tulisi palauttaa "Test passed!" Jos funktio toimii oikein, muuten se palauttaa "Test failed!" Kirjoita erilaisia testejä jokaiselle koodin osa-alueelle varmistaaksesi, että kaikki toimii odotetusti.

## Syvällisempi tarkastelu

Testien kirjoittaminen auttaa myös dokumentoimaan koodiasi ja helpottaa ymmärtämään sen toimintaa. Hyviä testejä kirjoittaessa sinun tulee ajatella erilaisia syötteitä ja miten ohjelmasi käsittelee niitä. Voit myös käyttää testaamiseen erilaisia kirjastoja, kuten JUnit C: lle.

## Katso myös

- [JUnit C](https://junit.org/junit5/docs/current/user-guide/#writing-tests)
- [Testaus C: ssä](https://gcc.gnu.org/onlinedocs/gcc-4.4.1/gcc/Testing.html)
- [TDD (Test Driven Development) C: ssä](https://dzone.com/articles/tdd-c-tutorial)