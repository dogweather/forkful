---
title:                "C: Ohjeistus testien kirjoittamiseen."
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa testejä?

Testien kirjoittaminen on tärkeä osa ohjelmointia, jota ei kannata jättää huomiotta. Se auttaa sinua varmistamaan, että koodisi toimii odotetulla tavalla ja vähentää virheiden esiintymistä tuotantokoodissa.

## Miten kirjoittaa testejä?

Testien kirjoittaminen C-kielellä on helppoa. Käytä vain testikirjastoa, kuten CUnit, ja kirjoita testit tapauksille, jotka haluat testata. Alla on esimerkki testikoodista sekä sen tulostuksesta:

```C
#include <stdio.h>
#include <CUnit/CUnit.h>

int add(int num1, int num2)
{
    return num1 + num2;
}

void test_add()
{
    CU_ASSERT(add(5, 10) == 15);
    CU_ASSERT(add(0, 0) == 0);
}

int main()
{
    CU_initialize_registry();
    CU_pSuite suite = CU_add_suite("add_suite", NULL, NULL);
    CU_add_test(suite, "test_add", test_add);
    CU_basic_run_suite(suite);
    CU_cleanup_registry();
    return 0;
}
```

Tulostus:

```
Suite: add_suite
  Test: test_add ... passed
  
Run Summary:  1/1 passed (100%)
```

## Syventävä sukellus

Kirjoittaminen testitapauksille ja niiden ajamiselle voi tuntua aikaa vievältä, mutta se on erittäin tärkeää pitkällä aikavälillä. Se auttaa sinua varmistamaan ohjelmasi laadun ja tehokkuuden, sekä vähentämään virheiden määrää ja parantamaan koodisi ylläpidettävyyttä.

Tärkeää on myös muistaa, että testien kirjoittaminen tilanteiden mukaan voi olla helpompi ja tehokkaampi tapa kuin jokaisen piirteiden testaaminen erikseen. Valitse siis testattavat tapaukset fiksusti ja varmista, että ohjelmasi toimii kaikissa mahdollisissa tilanteissa.

## Katso myös

- [CUnit-dokumentaatio](http://cunit.sourceforge.net/doc/writing_tests.html)
- [C-testikirjaston käyttö](https://overiq.com/c-programming-101/unit-testing-in-c-using-cunit/)
- [Parhaiden käytäntöjen opas testien kirjoittamiseen C:llä](https://hackernoon.com/unit-testing-in-c-best-practices-c7dcab3b948f)

Kiitos lukemisesta, toivottavasti tämä auttoi sinua ymmärtämään testien kirjoittamisen tärkeyttä C-ohjelmoinnissa. Onnea testaamiseen ja hyvien testikäytäntöjen noudattamiseen!