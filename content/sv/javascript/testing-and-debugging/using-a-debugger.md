---
date: 2024-01-26 03:50:38.186246-07:00
description: "H\xE4r \xE4r en bit JavaScript-kod som inte beter sig som f\xF6rv\xE4\
  ntat: ```javascript function buggyMultiply(a, b) { return a + b; // Hoppsan! Detta\
  \ borde vara en\u2026"
lastmod: '2024-03-13T22:44:38.297950-06:00'
model: gpt-4-0125-preview
summary: "H\xE4r \xE4r en bit JavaScript-kod som inte beter sig som f\xF6rv\xE4ntat:\
  \ ```javascript function buggyMultiply(a, b) { return a + b; // Hoppsan! Detta borde\
  \ vara en\u2026"
title: "Att anv\xE4nda en debugger"
weight: 35
---

## Hur man gör:
Här är en bit JavaScript-kod som inte beter sig som förväntat:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Hoppsan! Detta borde vara en multiplikation, inte addition.
}

let result = buggyMultiply(5, 3);
console.log('Resultat:', result);
```

Utdata är felaktig:
```
Resultat: 8
```

Låt oss felsöka i Chrome DevTools:

1. Öppna detta JS i en webbläsare.
2. Högerklicka och välj "Inspektera" för att öppna DevTools.
3. Klicka på fliken "Källor".
4. Hitta ditt kodsnutt eller sida och sätt en brytpunkt genom att klicka på radnumret bredvid `return`-satsen.
5. Uppdatera sidan för att utlösa brytpunkten.
6. Kontrollera panelen "Scope" för att se lokala variabler `a` och `b`.
7. Steg igenom med knappen "Steg över nästa funktionsanrop".
8. Upptäck buggen i `return`-satsen.
9. Fixa koden:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Fixat!
}

let result = buggyMultiply(5, 3);
console.log('Resultat:', result);
```

Den korrigerade utdatan:
```
Resultat: 15
```

## Djupdykning
Konceptet med felsökning har funnits sedan datorernas tidiga dagar—legenden säger att det började när en mal hittades i en dator på 1940-talet! Idag erbjuder JavaScript-debuggers som de inbyggda webbläsarverktygen (Chrome DevTools, Firefox Developer Tools) eller IDE-integrerade debuggers (Visual Studio Code, WebStorm) en mängd funktioner.

Alternativ till inbyggda debuggers inkluderar tredjepartverktyg som WebStorm eller att använda det goda gamla `console.log` för att skriva ut variabeltillstånd. Men dessa erbjuder inte den interaktion i realtid och detaljerade inspektion som debuggers ger.

När det gäller implementeringsdetaljer fungerar de flesta debuggers liknande: de låter dig sätta brytpunkter som pausar utförandet, stega igenom kod, inspektera nuvarande variabeltillstånd, bevaka uttryck och till och med manipulera värden på flygande fot för att testa olika scenarier.

## Se även
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - Felsökning](https://code.visualstudio.com/docs/editor/debugging)
