---
title:                "Testien kirjoittaminen"
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Testien kirjoittaminen on prosessi, jossa ohjelmoijat tarkistavat ja varmistavat, että heidän koodinsa toimii odotetulla tavalla. Se auttaa vähentämään virheitä ja parantaa koodin luotettavuutta ja laatua.

## Miten:

Esimerkki testin kirjoittamisesta:

```Arduino
//määritellään testattava funktio
int laskeSumma(int a, int b) {
  return a + b;
}

//huom. tässä ei ole määritelty mitään testejä, tätä koodia ei tule suorittaa!

```

Esimerkki testin kirjoittamisesta käyttäen Arduino testikirjastoa:

```Arduino
#include <ArduinoUnit.h>  //lisätään testikirjasto

//testaa laskeSumma-funktion palauttamaa arvoa
test(summatTesti) {
  assertEqual(laskeSumma(2,3), 5);
  assertNotEqual(laskeSumma(2,3), 6);
}

//testaa laskeSumma-funktion käsittelyä negatiivisilla luvuilla
test(negatiivisetLuvut) {
  assertEqual(laskeSumma(-2,3), 1);
  assertNotEqual(laskeSumma(-2,3), -1);
}

// "print"-komento tulostaa testin tulokset sarjalähtöporttiin
unittest_main();
```

Sample output:

```
TEST: summatTesti
PASS

TEST: negatiivisetLuvut
PASS

Suoritetut testit: 2
Testit onnistuneesti läpäisty: 2
Testit epäonnistuneesti: 0
```

## Syvällisemmin:

Testien kirjoittamisella on pitkä historia ohjelmistokehityksessä ja se on vakiintunut käytäntö monissa ohjelmointikielissä. On myös olemassa muita testikirjastoja kuten googletest, jotka tarjoavat samanlaisia toimintoja kuin Arduino testikirjasto. Testien kirjoittaminen auttaa myös dokumentoimaan ja ymmärtämään koodia paremmin sekä helpottaa tulevia muutoksia ja lisäyksiä.

## Katso myös:

- [Arduino Unit Test Library] (https://github.com/mmurdoch/arduinounit)