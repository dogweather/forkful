---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:52.888017-07:00
description: "Refactoring is het proces van het wijzigen van de interne structuur\
  \ van een computerprogramma zonder het externe gedrag aan te passen. Programmeurs\
  \ doen\u2026"
lastmod: '2024-03-13T22:44:51.121255-06:00'
model: gpt-4-0125-preview
summary: "Refactoring is het proces van het wijzigen van de interne structuur van\
  \ een computerprogramma zonder het externe gedrag aan te passen. Programmeurs doen\u2026"
title: Refactoring
weight: 19
---

## Wat & Waarom?

Refactoring is het proces van het wijzigen van de interne structuur van een computerprogramma zonder het externe gedrag aan te passen. Programmeurs doen dit om hun code op te schonen, waardoor het gemakkelijker te begrijpen, te onderhouden en uit te breiden is.

## Hoe te:

Stel je voor dat je een functie hebt die net iets te veel doet, zoals deze logge methode die een object initialiseert en ook voor het loggen zorgt:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Initialisatielogica
        // ...

        // Uitgebreid loggen
        if (verbose) {
            std::cout << "Widget geïnitialiseerd!" << std::endl;
        }
    }
};

// Gebruik:
Widget w;
w.init(true);
```

Uitvoer:
```
Widget geïnitialiseerd!
```

Refactoring hiervan naar schonere, meer gefocuste methodes ziet er mogelijk zo uit:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Alleen initialisatielogica
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget geïnitialiseerd!" << std::endl;
    }
};

// Gebruik:
Widget w;
w.init();
w.logInitialization();
```

Deze verandering heeft niet veranderd wat het programma doet, maar maakt de `Widget` klasse modulairder en het gebruik ervan duidelijker.

## Diepe Duik

Het concept van refactoring zoals we dat vandaag de dag kennen, vindt zijn oorsprong in de Smalltalk-programmeergemeenschappen van de jaren '80 en werd sterk gepopulariseerd door Martin Fowlers boek "Refactoring: Improving the Design of Existing Code" uit 1999. Vandaag de dag is refactoring een kernonderdeel van de moderne softwareontwikkeling, geïntegreerd in verschillende ontwikkelmethodologieën zoals Agile en TDD (Test-Driven Development).

Wanneer we praten over alternatieven voor refactoring, belanden we in het gebied van herontwerpen of herprogrammeren. Refactoring is strategisch en incrementeel, terwijl een herontwerp bestaande code kan schrappen ten gunste van een nieuwe oplossing. Herontwerp, ondertussen, kan meer significante wijzigingen met zich meebrengen, waaronder het veranderen van functionaliteit, wat geen doel is voor puur refactoring.

Implementatiedetails over refactoring kunnen heel gedetailleerd worden. Er zijn veel 'code smells' die kunnen leiden tot een refactor, zoals lange methoden, grote klassen of gedupliceerde code. Er bestaan geautomatiseerde hulpmiddelen die kunnen helpen bij refactoring, zoals "Clang-Tidy" voor C++, die problemen kan opsporen en zelfs enkele fixes kan toepassen.

Bovendien vereist refactoring een solide reeks tests om ervoor te zorgen dat de functionaliteit ongewijzigd blijft. Zonder tests vlieg je in feite blind en riskeer je regressies.

## Zie ook

Voor een dieper begrip van refactoring en om meer voorbeelden te zien, wil je misschien ook kijken naar:

- De klassieke tekst van Martin Fowler "Refactoring: Improving the Design of Existing Code" voor fundamentele ideeën en strategieën.
- De `Clang-Tidy` documentatie op https://clang.llvm.org/extra/clang-tidy/ voor geautomatiseerde refactoring-ondersteuning in C++.
- "Working Effectively with Legacy Code" van Michael Feathers, dat technieken biedt voor veilige refactoring in de context van minder perfecte bestaande codebases.
