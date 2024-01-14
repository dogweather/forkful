---
title:    "C: Testien kirjoittaminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa hyvän ja toimivan C-ohjelman kehittämistä. Ne auttavat tunnistamaan ja korjaamaan mahdollisia virheitä ennen kuin ohjelma julkaistaan, mikä parantaa sen luotettavuutta ja vähentää käyttäjien kohtaamien ongelmien määrää.

## Miten

Ohjelmistotestauksen tärkein periaate on, että jokaisen funktion, luokan tai moduulin tulee olla itsenäisesti testattavissa. Tämä tarkoittaa, että koodin on oltava kirjoitettu siten, että sen toimivuutta voidaan testata erillisellä testikoodilla ilman muiden osien riippuvuutta. Seuraavassa esimerkissä näytetään yksinkertainen testi funktiolle, joka palauttaa kahden luvun keskiarvon:

```C
#include <stdio.h>

double keskiarvo(int a, int b) {
  return (a + b) / 2.0;
}

int main() {
  double tulos = keskiarvo(5, 10);
  printf("Keskiarvo on: %.2f", tulos);

  return 0;
}
```

Tämän testin tulisi palauttaa seuraava tulos:

```
Keskiarvo on: 7.50
```

Tämän testin avulla voidaan varmistua siitä, että keskiarvon laskeminen toimii oikein ja että se palauttaa halutun tuloksen.

## Syväsyventyminen

Testien kirjoittamisen tarkoituksena on tarkastella ohjelman eri osia ja varmistaa niiden toimivuus. Tämä tarkoittaa, että sinun tulisi testata kaikki funktiot, luokat ja moduulit erikseen sekä myös niiden yhteistoimivuus. Lisäksi ohjelman eri toiminnallisuuksien on oltava testattuja ja toimintavarmoja kaikissa tilanteissa.

Testikoodin kirjoittaminen voi myös auttaa sinua paremmin ymmärtämään ohjelmasi toimintaa ja tunnistamaan mahdollisia ongelmallisia alueita. Lisäksi testien avulla voit varmistaa, että muutokset ohjelmassa eivät aiheuta uusia ongelmia ja että ohjelma toimii edelleen odotetusti.

Nykypäivänä on olemassa useita testikehyksiä, kuten esimerkiksi CUnit, joiden avulla voit helposti kirjoittaa ja suorittaa testejä ohjelmassasi. Suosittelemme tutustumaan näihin ja hyödyntämään niitä testikoodin kirjoittamisessa.

## Katso myös

- [CUnit-testikehys](https://www.cs.umd.edu/~srhuang/teaching/cmsc212/gdb-tutorial-handout.pdf)
- [Hyödyllisiä vinkkejä C-ohjelmointiin](https://www.macs.hw.ac.uk/~hwloidl/Courses/F21SC/LEC01/LECTURE1.pdf)
- [Miten kirjoittaa tehokasta testikoodia](https://www.guru99.com/unit-testing-guide.html)