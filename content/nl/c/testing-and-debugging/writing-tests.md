---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:52.170731-07:00
description: "Tests schrijven in C betreft het cre\xEBren van kleinere, hulpprogramma's\
  \ of functies die automatisch de functionaliteit van je code verifi\xEBren. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:51.294860-06:00'
model: gpt-4-0125-preview
summary: "Tests schrijven in C betreft het cre\xEBren van kleinere, hulpprogramma's\
  \ of functies die automatisch de functionaliteit van je code verifi\xEBren."
title: Tests Schrijven
weight: 36
---

## Wat & Waarom?
Tests schrijven in C betreft het creëren van kleinere, hulpprogramma's of functies die automatisch de functionaliteit van je code verifiëren. Programmeurs doen dit om te zorgen dat hun software werkt zoals verwacht, om fouten vroegtijdig op te sporen, en om toekomstige code wijzigingen te vergemakkelijken zonder onbedoelde bijeffecten.

## Hoe:
Hoewel C geen ingebouwd testframework heeft zoals sommige andere talen, kun je nog steeds effectieve tests schrijven met `assert.h` voor eenvoudige beweringen of door derde partij frameworks zoals CUnit of Unity te integreren voor meer gestructureerd testen. Hier is een basisvoorbeeld met `assert.h` om een functie te testen die twee integers optelt:

```c
#include <assert.h>
#include "my_math.h"

void test_addition() {
    assert(add(1, 2) == 3);
    assert(add(-1, -2) == -3);
    assert(add(0, 0) == 0);
    printf("Alle opteltests geslaagd.\n");
}

int main() {
    test_addition();
    return 0;
}
```

In `my_math.h`, zou je kunnen hebben:

```c
// Eenvoudige optelfunctie
int add(int a, int b) {
    return a + b;
}
```

Het uitvoeren van de testfunctie in je `main` functie geeft uit:

```
Alle opteltests geslaagd.
```

Voor een uitgebreidere testopzet met een framework zoals Unity, zou je het framework in je project integreren, en vervolgens testgevallen op een vergelijkbare manier schrijven, maar met gebruikmaking van de API van het framework voor beweringen en het uitvoeren van tests.

## Diepgaand
Testen in C is historisch gezien een handmatig en enigszins ad hoc proces geweest vanwege de low-level aard van de taal en het ontbreken van een gestandaardiseerd testframework. Deze handmatige aanpak leidde vaak tot minder grondige testpraktijken in vergelijking met talen met ingebouwde testondersteuning. Aangezien de C-taal cruciaal is geweest in de ontwikkeling van fundamentele softwaresystemen, leidde dit gebrek aan formele testframeworks ertoe dat de C-gemeenschap derde partij oplossingen ontwikkelde, zoals CUnit en Unity.

Deze tools, hoewel extern aan de standaard C-bibliotheek, bieden functionaliteit vergelijkbaar met testframeworks in andere talen, en bieden een gestructureerde manier om tests te definiëren, uit te voeren en te evalueren. Ze helpen de kloof te overbruggen tussen C's krachtige systeemniveau-toegang en de moderne ontwikkelpraktijk van geautomatiseerd testen. Het is vermeldenswaardig dat, hoewel deze tools het testproces in C aanzienlijk verbeteren, ze een leercurve kunnen introduceren en de complexiteit van de projectopzet kunnen verhogen in vergelijking met talen met geïntegreerde testondersteuning. Daarom is, voor projecten waar betrouwbaarheid en onderhoudbaarheid van het grootste belang zijn, de investering in het opzetten van een geschikte testomgeving in C wel gerechtvaardigd, zelfs met het oog op mogelijke alternatieven.
