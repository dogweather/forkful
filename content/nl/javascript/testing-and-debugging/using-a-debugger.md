---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:45.299623-07:00
description: "Hier is een stukje JavaScript-code dat zich niet gedraagt zoals verwacht:\
  \ ```javascript function buggyMultiply(a, b) { return a + b; // Oeps! Dit zou een\u2026"
lastmod: '2024-03-13T22:44:51.206348-06:00'
model: gpt-4-0125-preview
summary: "Hier is een stukje JavaScript-code dat zich niet gedraagt zoals verwacht:\n\
  \n```javascript\nfunction buggyMultiply(a, b) {\n    return a + b; // Oeps."
title: Een debugger gebruiken
weight: 35
---

## Hoe:
Hier is een stukje JavaScript-code dat zich niet gedraagt zoals verwacht:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Oeps! Dit zou een vermenigvuldiging moeten zijn, geen optelling.
}

let result = buggyMultiply(5, 3);
console.log('Resultaat:', result);
```

De uitvoer is incorrect:
```
Resultaat: 8
```

Laten we debuggen in Chrome DevTools:

1. Open deze JS in een browser.
2. Klik met de rechtermuisknop en selecteer "Inspecteren" om DevTools te openen.
3. Klik op het tabblad "Bronnen".
4. Vind je codefragment of pagina en zet een breekpunt door op het regelnummer naast de `return`-instructie te klikken.
5. Vernieuw de pagina om het breekpunt te activeren.
6. Controleer het "Scope" paneel om de lokale variabelen `a` en `b` te zien.
7. Ga door met de "Volgende functieaanroep overslaan" knop.
8. Vind de fout in de `return`-instructie.
9. Corrigeer de code:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Gecorrigeerd!
}

let result = buggyMultiply(5, 3);
console.log('Resultaat:', result);
```

De gecorrigeerde uitvoer:
```
Resultaat: 15
```

## Diepe Duik
Het concept van debuggen bestaat al sinds de vroege dagen van de informatica—de legende zegt dat het begon toen er een mot werd gevonden in een computer in de jaren 1940! Vandaag de dag bieden JavaScript debuggers zoals de ingebouwde browser tools (Chrome DevTools, Firefox Developer Tools) of IDE-geïntegreerde debuggers (Visual Studio Code, WebStorm) een heleboel functies.

Alternatieven voor ingebouwde debuggers zijn onder andere tools van derden zoals WebStorm of het gebruik van de goede oude `console.log` om de toestanden van variabelen te outputten. Maar deze bieden niet de real-time interactie en gedetailleerde inspectie die debuggers bieden.

Wat betreft de implementatiedetails, de meeste debuggers werken op een vergelijkbare manier: ze stellen je in staat om breekpunten in te stellen die de uitvoering pauzeren, stap voor stap door de code te gaan, de huidige toestand van variabelen te inspecteren, expressies te bekijken en zelfs waarden on the fly te manipuleren om verschillende scenario's te testen.

## Zie Ook
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/nl/docs/Tools/Debugger)
- [Visual Studio Code - Debuggen](https://code.visualstudio.com/docs/editor/debugging)
