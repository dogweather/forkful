---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:09.494018-07:00
description: "Testien kirjoittaminen C++:ssa tarkoittaa pienten, itsen\xE4isten ohjelmien\
  \ luomista, jotka automaattisesti varmistavat koodikannan osien toiminnan.\u2026"
lastmod: '2024-03-13T22:44:56.868432-06:00'
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen C++:ssa tarkoittaa pienten, itsen\xE4isten ohjelmien\
  \ luomista, jotka automaattisesti varmistavat koodikannan osien toiminnan.\u2026"
title: Testien kirjoittaminen
---

{{< edit_this_page >}}

## Mikä ja miksi?

Testien kirjoittaminen C++:ssa tarkoittaa pienten, itsenäisten ohjelmien luomista, jotka automaattisesti varmistavat koodikannan osien toiminnan. Ohjelmoijat tekevät näin varmistaakseen, että heidän koodinsa toimii odotetulla tavalla, estääkseen regressiot (eli uusien muutosten rikkovan olemassa olevaa toiminnallisuutta) ja helpottaakseen ylläpidettävien koodikantojen hallintaa ajan myötä.

## Kuinka:

### Käyttäen Google Test -kehystä

Yksi suosituimmista kolmannen osapuolen kirjastoista C++:n testien kirjoittamiseen on Google Test. Ensin sinun tulee asentaa Google Test ja linkittää se projektiisi. Kun olet valmis, voit alkaa kirjoittaa testitapauksia.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Tallenna koodi tiedostoon ja käännä se g++-kääntäjällä, linkittaen Google Test -kirjasto. Jos kaikki on asetettu oikein, tuloksena olevan suoritettavan tiedoston suorittaminen ajaa testin, ja jos `add`-funktio toimii odotetulla tavalla, näet jotain seuraavaa:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Käyttäen Catch2:ta

Toinen suosittu testauskehys C++:lle on Catch2. Siinä on yksinkertaisempi syntaksi eikä se yleensä vaadi linkittämistä kirjastoa vasten (vain otsikkotiedosto). Tässä on esimerkki yksinkertaisen testin kirjoittamisesta Catch2:lla:

```cpp
#define CATCH_CONFIG_MAIN  // Tämä kertoo Catchille että se tarjoaa main()-funktion - tee näin vain yhdessä cpp-tiedostossa
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Kokonaislukuja kertolasketaan", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Kääntämisen ja tämän testin suorittamisen jälkeen Catch2 tarjoaa selkeän tulosteen, joka osoittaa, jäikö testi läpäiseväksi tai hylätyksi, mukaan lukien kaikki vianjäljitykseen tarvittavat tiedot:

```
===============================================================================
Kaikki testit läpäisty (1 väite 1 testitapauksessa)
```

Nämä esimerkit näyttävät, kuinka testauskehyksien integroiminen C++-kehitystyönkulkuusi voi merkittävästi parantaa koodisi luotettavuutta ja ylläpidettävyyttä.
