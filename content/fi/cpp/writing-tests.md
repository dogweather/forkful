---
title:    "C++: Testien kirjoittaminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi: Testien kirjoittamisen tärkeys ohjelmoinnissa

Testien kirjoittaminen on tärkeä osa ohjelmointiprosessia, koska se auttaa varmistamaan, että koodi toimii odotetulla tavalla ja vähentää mahdollisia virheitä. Testien avulla voidaan myös helposti havaita ja korjata ohjelmistossa olevia ongelmia, mikä säästää aikaa ja vaivaa pitkällä aikavälillä.

## Kuinka tehdä: Esimerkkejä ja koodinpätkiä testien kirjoittamisesta

```C++
#include <iostream>
#include <cassert>

int sum(int a, int b){
  return a + b;
}

int main(){
  // Testataan sum-funktion toimivuus
  assert(sum(2,2) == 4);
  assert(sum(-3,5) == 2);

  std::cout << "Testit läpäisty!" << std::endl;
  return 0;
}
```

Kooditestejä voidaan kirjoittaa erilaisilla testikehyksillä, kuten CppUnit tai Google Test. Näillä kehyksillä voidaan testien suorittamisen lisäksi myös raportoida automaattisesti mahdollisista virheistä.

## Syvällisempi perehtyminen testien kirjoittamiseen

Testien kirjoittamisessa on hyvä noudattaa TDD (Test Driven Development) -menetelmää, jossa testit kirjoitetaan ennen varsinaista koodia. Näin varmistetaan, että koodi toimii halutulla tavalla ja mahdolliset virheet havaitaan aikaisessa vaiheessa.

Hyödyllisiä testattavia alueita ovat esimerkiksi rajapinnat, paluuarvot ja reunaehtojen käsittely. Myös erilaisia virhetilanteita kannattaa testata, jotta varmistetaan ohjelman toimivuus mahdollisissa poikkeustilanteissa.

Testien kirjoittaminen vaatii myös taitoa ja kokemusta, joten niiden tekemistä kannattaa harjoitella ja opetella jatkuvasti. Lisäksi on tärkeää huolehtia testien ylläpidosta ja päivittämisestä uusien ominaisuuksien tai muutosten yhteydessä.

## Katso myös

- [CppUnit](https://cppunit.sourceforge.io/)
- [Google Test](https://github.com/google/googletest)

Loppujen lopuksi testien kirjoittaminen on investointi ohjelmiston laatuun ja helpottaa pitkällä aikavälillä kehittäjän työtä. Suosittelemme siis vahvasti testien sisällyttämistä osaksi ohjelmointiprosessia.