---
aliases:
- /nl/google-apps-script/using-a-debugger/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:01.454244-07:00
description: "Debuggen in Google Apps Script (GAS) betreft het proces van het identificeren\
  \ en verwijderen van fouten uit scripts die bedoeld zijn om Google Apps te\u2026"
lastmod: 2024-02-18 23:09:01.391025
model: gpt-4-0125-preview
summary: "Debuggen in Google Apps Script (GAS) betreft het proces van het identificeren\
  \ en verwijderen van fouten uit scripts die bedoeld zijn om Google Apps te\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?

Debuggen in Google Apps Script (GAS) betreft het proces van het identificeren en verwijderen van fouten uit scripts die bedoeld zijn om Google Apps te automatiseren of webapplicaties te bouwen. Programmeurs debuggen om ervoor te zorgen dat hun code uitvoert zoals verwacht, wat de betrouwbaarheid en prestaties van applicaties verbetert.

## Hoe te:

Google Apps Script biedt een ingebouwde debugger binnen de Apps Script Editor om scripts te helpen troubleshooten. Hier is hoe je de debugger initieert en gebruikt:

1. **Open je script in de Apps Script Editor.**
2. **Selecteer een functie om te debuggen.** Vanuit het dropdownmenu bovenaan selecteer je de functie die je wilt debuggen.
3. **Stel breakpoints in.** Klik op de gutter (het grijze gebied links van de regelnummers) waar je de uitvoering wilt pauzeren; er verschijnt een rode stip, wat een breakpoint aangeeft.
4. **Start met debuggen.** Klik op het bug-icoon of selecteer `Debug` > `Start debugging`. De uitvoering start en pauzeert bij het eerste breakpoint.

Overweeg dit eenvoudige script:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Bedoeld om 15 te loggen
}
```

Als je niet zeker weet waarom `Logger.log(sum)` niet het verwachte resultaat weergeeft, zou je een breakpoint kunnen instellen op de regel `var sum = a + b;` en vervolgens regel voor regel door het script stappen om de waarden van variabelen te inspecteren.

**Voorbeelduitvoer in Logger:**

```plain
15
```

Tijdens het debuggen stelt de Apps Script Editor je in staat om:

- **Door de code te stappen** met de knoppen voor stap over, stap in en stap uit.
- **Expressies en variabelen te bekijken** om hun waarden in real time te zien veranderen.
- **De call stack te inspecteren** om functieaanroepen te traceren.

## Diepere Duik

Debuggen in Google Apps Script, net als in elke andere programmeeromgeving, is essentieel voor het creëren van foutloze applicaties. Geïntroduceerd in de vroege ontwikkeling van GAS, biedt de ingebouwde debugger fundamentele mogelijkheden om code stapsgewijs te inspecteren en te repareren. Hoewel het basisfuncties voor debuggen biedt, vergelijkbaar met die in meer volwassen omgevingen zoals Visual Studio Code of IntelliJ, kan het tekortschieten voor complexe debugscenario's. Zo kunnen de mogelijkheden om asynchrone callbacks te inspecteren of het beheren van zware scriptuitvoeringen beperkend zijn.

Voor complexe debugbehoeften zouden ontwikkelaars kunnen overgaan op alternatieve methoden zoals uitgebreide logging (met `Logger.log()`) of zelfs deployeren als een webapp om gedrag in een realistisch scenario te inspecteren. Echter, de eenvoud en integratie van GAS's debugger binnen de Apps Script Editor maken het een onschatbare eerste stap voor het troubleshooten en begrijpen van scriptgedrag. Opvallend is dat met de continue updates en verbeteringen van Google aan Apps Script, de debug-ervaring gestaag verbetert en biedt mettertijd geavanceerdere tools en opties. Deze evolutie weerspiegelt Google's toewijding om Apps Script een krachtiger en toegankelijker platform te maken voor ontwikkelaars van diverse achtergronden.
