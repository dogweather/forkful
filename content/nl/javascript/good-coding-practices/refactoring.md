---
title:                "Refactoring"
aliases: - /nl/javascript/refactoring.md
date:                  2024-01-28T22:06:13.048722-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herstructureren van bestaande computercode zonder het externe gedrag ervan te veranderen. Programmeurs doen dit om de niet-functionele attributen van de software te verbeteren, waardoor de code schoner en efficiënter wordt, wat op zijn beurt het onderhoud vereenvoudigt en toekomstige functietoevoegingen gemakkelijker maakt.

## Hoe te:

Laten we kijken naar een eenvoudig voorbeeld waar refactoring uw code beknopter en leesbaarder kan maken. Hier, herstructureren we een functie die de som van een reeks getallen berekent.

Voor:
```javascript
function calculateSum(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}

console.log(calculateSum([1, 2, 3, 4])); // Uitvoer: 10
```

Na:
```javascript
function calculateSum(arr) {
  return arr.reduce((sum, num) => sum + num, 0);
}

console.log(calculateSum([1, 2, 3, 4])); // Uitvoer: 10
```

Zie je hoe de `reduce` methode de grootte van de functie vermindert terwijl de functionaliteit intact blijft? Dat is refactoring voor jou.

## Diepgaande Duik

Refactoring kwam pas als een formele praktijk naar voren met de publicatie van Martin Fowlers boek "Refactoring: Improving the Design of Existing Code" in 1999. Dit boek, samen met de opkomst van agile softwareontwikkeling, hielp refactoring naar de mainstream te duwen.

Refactoring beschrijven als een aspect van softwareontwikkeling is zoals uitleggen waarom je een werkplaats zou opruimen: je doet het zodat de volgende keer dat je iets moet repareren (in dit geval code), je minder tijd besteedt aan het omgaan met de rommel en meer aan het eigenlijke probleem.

Wanneer we het hebben over alternatieven voor refactoring, betreden we een bredere discussie over strategieën voor softwareonderhoud. Men zou bijvoorbeeld kunnen kiezen voor een volledige herschrijving, maar dat is vaak kostbaarder en riskanter. Refactor incrementeel, en je plukt voortdurend de vruchten zonder het schip te laten zinken door een plotselinge overhaal.

Refactoring is geholpen door de ontwikkeling van geïntegreerde ontwikkelomgevingen (IDE's) en hulpmiddelen zoals JSHint, ESLint en Prettier in het JavaScript-ecosysteem, die automatische codekwaliteitscontroles uitvoeren en mogelijkheden voor refactoring benadrukken.

Het draait allemaal om schone, expressieve en onderhoudbare code. Geavanceerde algoritmen, optimalisaties van datastructuren of zelfs architectonische veranderingen zoals overschakelen van procedurele naar functionele programmeerstijlen kunnen deel uitmaken van een refactoringproces.

Refactoring moet zorgvuldig worden gedaan; het is essentieel om een robuuste set tests te hebben om ervoor te zorgen dat je wijzigingen het gedrag van de software niet onverwacht hebben veranderd - nog een reden waarom Testgedreven Ontwikkeling (TDO) mooi aansluit bij refactoring, aangezien het standaard die veiligheidsnet biedt.

## Zie Ook

- Martin Fowlers Refactoring Boek: [Refactoring - Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- JavaScript Test Frameworks (om ervoor te zorgen dat refactoring de functionaliteit niet breekt):
  - Jest: [Jest - Aangenaam JavaScript Testen](https://jestjs.io/)
  - Mocha: [Mocha - het leuke, simpele, flexibele JavaScript testraamwerk](https://mochajs.org/)

- Hulpmiddelen voor Codekwaliteit en Ondersteuning bij Refactoring:
  - ESLint: [ESLint - Aanpasbare JavaScript linter](https://eslint.org/)
  - Prettier: [Prettier - Eigengereide Code Formatter](https://prettier.io/)
