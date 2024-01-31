---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testit ovat koodeja jotka tarkistavat ohjelmasi käyttäytymisen automaattisesti. Testauksesta on tullut ohjelmoinnin peruskivi koska se vahvistaa koodin oikeellisuutta, havaitsee virheet ajoissa ja helpottaa kunnossapitoa.

## How to:
```C++
#include <cassert>

// Funktion esittely
int summa(int a, int b) {
    return a + b;
}

// Testifunktio
void testaa_summa() {
    assert(summa(1, 2) == 3);
    assert(summa(-1, -2) == -3);
    assert(summa(0, 0) == 0);

    // Tulostetaan "Testit ok!", jos edelliset assert-lauseet eivät aiheuta virhettä
    std::cout << "Testit ok!" << std::endl;
}

int main() {
    testaa_summa(); // Testien ajaminen
    return 0;
}
```
Output:
```
Testit ok!
```

## Deep Dive
Alun perin ohjelmoinnin alkuaikoina testaus oli manuaalista ja aikaa vievää. Nykyään on olemassa monia testaustyökaluja kuten Google Test C++:lle. Vaihtoehtoisesti voit käyttää TDD (Test-Driven Development) periaatetta, missä luot testit ennen itse koodia. Käytännön toteutus testeissä vaihtelee unit-testeistä integraatio- ja hyväksymistesteihin, jotka kaikki tähtäävät ohjelman laadun varmistamiseen eri tasoilla.

## See Also
- [Google Test GitHub](https://github.com/google/googletest) - Google Test, C++ testaustyökalu
- [C++ Reference Testing](http://www.cplusplus.com/reference/cassert/) - C++ `cassert` kirjaston dokumentaatio
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development) - Lisätietoa TDD:stä
