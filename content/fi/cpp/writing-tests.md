---
title:                "Testien kirjoittaminen"
html_title:           "C++: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi

Kirjoittaa testejä voi aluksi tuntua turhalta ja aikaa vievältä, mutta loppujen lopuksi se auttaa säästämään aikaa ja vaivaa suuremmilta ongelmilta. Testien kirjoittaminen varmistaa, että koodi on toiminnassa ja mahdolliset virheet havaitaan ja korjataan ennen kuin ne aiheuttavat suurempia ongelmia käytössä.

# Miten

Testien kirjoittaminen C++ ohjelmointikielessä on suhteellisen yksinkertaista käyttäen esimerkiksi JUnit tai Google Test kirjastoja. Testit kirjoitetaan omiin tiedostoihin ja niiden avulla testataan yksittäisiä funktioita ja luokkia. Alla on esimerkki yksinkertaisesta testitiedostosta:

```C++
#include <gtest/gtest.h> // Testikirjasto

// Luodaan luokka, jonka funktioita halutaan testata
class Calculator {
    int add(int a, int b) {
        return a + b;
    }
};

// Luodaan testi, joka tarkistaa add-funktion toimivuuden
TEST (CalculatorTest, add) {
    // Luodaan luokan instanssi
    Calculator calc;
    // Testataan, että add-funktio palauttaa oikean arvon
    EXPECT_EQ(calc.add(2, 3), 5);
}

int main(int argc, char **argv) {
    // Suoritetaan testit
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Testit voidaan suorittaa esimerkiksi komentoriviltä käyttäen komentoa ```g++ testitiedosto.cpp -lgtest``` ja suorittaa sen jälkeen luotu ohjelma.

# Syväsukellus

Testien kirjoittaminen auttaa myös dokumentoimaan koodia ja parantamaan sen ymmärrettävyyttä. Testien avulla voidaan varmistaa myös koodin jatkuvaa toimivuutta muutosten tai päivitysten jälkeen. Testien kirjoittaminen vaatii kuitenkin aikaa ja vaivaa, mutta se auttaa välttämään suuremmat ongelmat ja parantaa ohjelmiston laatua.

# Katso myös

- [JUnit-kirjasto](https://junit.org)
- [Google Test -kirjasto](https://github.com/google/googletest)