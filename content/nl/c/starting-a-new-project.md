---
title:                "Een nieuw project starten"
date:                  2024-01-28T22:08:33.725943-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een nieuw project starten in C betekent het opzetten van een basisstructuur voor je app. Programmeurs doen dit om een fundament te leggen, zodat alles wat volgt een nette plek heeft om te leven.

## Hoe te:
```C
#include <stdio.h>

int main() {
    printf("Hallo, nieuw project!\n");
    return 0;
}
```

Voer het uit, en je zou moeten zien:
```
Hallo, nieuw project!
```

## Diepgaand
Terug in de jaren '70 werd C geboren. Dennis Ritchie begon iets groots bij Bell Labs. De eenvoud van C maakt het zelfs nu nog een eerste keus voor systeemsoftware, ingebedde systemen en applicaties met hoge prestaties.

Als je begint, kies tussen procedurele of modulaire stijlen. Procedureel is eenvoudig, vergelijkbaar met het volgen van een recept. Modulair laat je code in stukken organiseren – denk aan ingrediënten gesorteerd in kommetjes. Beide werken, maar modulair schaalt beter voor complexe projecten.

Onder de motorkap, wanneer je compileert, wordt je opzet omgezet in een uitvoerbaar bestand. De compiler (zoals GCC) leest je `main()` functie als het startpunt. Maar er is meer: bibliotheken koppelen, makefiles opzetten voor grotere projecten, en misschien wat preprocessor richtlijnen toevoegen voor de smaak.

## Zie Ook
- [GNU GCC Compiler](https://gcc.gnu.org/)
- [Makefile Tutorial](https://makefiletutorial.com/)
