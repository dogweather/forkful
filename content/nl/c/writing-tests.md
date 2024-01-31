---
title:                "Tests Schrijven"
date:                  2024-01-28T22:12:50.159656-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven betekent code maken die controleert of je andere code werkt. Programmeurs doen dit om vroegtijdig bugs op te sporen, tijd te besparen en ervoor te zorgen dat code nu werkt en ook later blijft werken.

## Hoe:

In C kun je tests schrijven door gebruik te maken van een testframework zoals CUnit of door je eigen eenvoudige testsysteem op te zetten. Hieronder staat een basaal voorbeeld met gebruik van assert om een basis testfunctie voor een `add` functie te creëren.

```C
#include <assert.h>

// Functie om te testen
int add(int a, int b) {
    return a + b;
}

// Testfunctie
void test_add() {
    assert(add(2, 2) == 4);
    assert(add(-1, 1) == 0);
}

int main() {
    test_add();
    printf("Alle tests geslaagd!\n");
    return 0;
}
```

Voorbeelduitvoer, als alle tests zijn geslaagd:

```
Alle tests geslaagd!
```

Als een test mislukt, wordt het programma afgebroken en wordt er een foutmelding geprint.

## Diepere Duik

Historisch gezien kwam C niet met een ingebouwd testframework. Programmeurs schreven meestal aangepaste testfuncties of gebruikten frameworks van derden. Populaire frameworks zijn CUnit, Check en Unity. Elk biedt functies zoals automatische testdetectie, setup/afbraak processen en rapportage van testresultaten. Voor kleine projecten kunnen eenvoudige assert-gebaseerde tests volstaan, maar naarmate de complexiteit toeneemt, bespaart een geschikt framework tijd en moeite.

## Zie Ook

Hier zijn enkele nuttige links voor meer verdiepende duiken:

- [CUnit](http://cunit.sourceforge.net/)
- [Check](https://libcheck.github.io/check/)
- [Unity](http://www.throwtheswitch.org/unity)
- [Assert.h in C programmeren](https://www.tutorialspoint.com/assert-h-in-c-programming)
