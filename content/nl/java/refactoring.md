---
title:                "Refactoring"
aliases:
- nl/java/refactoring.md
date:                  2024-01-28T22:05:39.775246-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herstructureren van bestaande computercode - het wijzigen van de factoring - zonder het externe gedrag ervan te veranderen. Programmeurs doen dit om de niet-functionele attributen van de software te verbeteren, zoals het verhogen van de leesbaarheid, het verminderen van de complexiteit en het codeerwerk onderhoudbaarder te maken voor toekomstige projecten.

## Hoe:
Laten we een eenvoudige Java-klasse nemen die schreeuwt om refactoring vanwege de slechte organisatie en het gebrek aan duidelijkheid.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Andere operaties...
    }
}
```

Na het refactoren hebben we:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Andere operaties...
}
```

Door te refactoren hebben we de methodenamen en parameters verbeterd voor leesbaarheid en de noodzaak voor een voorwaardelijke vertakking binnen een enkele methode verwijderd. Elke operatie geeft nu duidelijk haar doel aan.

## Diepere Duik:
Refactoring heeft zijn wortels in de Smalltalk-gemeenschap, met de nadruk op code leesbaarheid en object-georiënteerd ontwerp, maar het nam echt een vlucht in de Java-wereld in de late jaren '90 en vroege jaren '00, vooral na de publicatie van Martin Fowler's baanbrekende boek, "Refactoring: Improving the Design of Existing Code."

Er zijn alternatieven voor refactoring, zoals het opnieuw schrijven van code vanaf nul. Echter, refactoring wordt vaak de voorkeur gegeven omdat het incrementele veranderingen inhoudt die de functionaliteit van de applicatie niet verstoren.

Implementatiedetails bij het refactoren in Java (of elke programmeertaal) draaien om het begrijpen van code smells - indicatoren van diepere problemen in de code. Sommige smells omvatten lange methoden, grote klassen, dubbele code en overmatig gebruik van primitieven. Door refactoring-patronen toe te passen zoals Extract Method, Move Method, of Replace Temp with Query, kunnen ontwikkelaars deze smells systematisch aanpakken terwijl ze ervoor zorgen dat de code te allen tijde functioneel blijft.

Geautomatiseerde tools, zoals de refactoringondersteuning van IntelliJ IDEA, of plugins voor Eclipse, kunnen het proces ondersteunen door automatische refactorings uit te voeren zoals het hernoemen van variabelen, methoden en klassen, het extraheren van methoden of variabelen, en het verplaatsen van methoden of klassen naar verschillende pakketten of namespaces.

## Zie Ook:
- Martin Fowlers "Refactoring: Improving the Design of Existing Code": https://martinfowler.com/books/refactoring.html
- Refactoring-technieken op Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- Geautomatiseerde refactoring in Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- IntelliJ IDEA's refactoring-functies: https://www.jetbrains.com/idea/features/refactoring.html

Elk van deze bronnen biedt ofwel een basis voor het begrijpen van de principes van refactoring of tools die kunnen worden ingezet om deze principes in de praktijk te brengen.
