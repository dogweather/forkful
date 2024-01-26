---
title:                "Att använda en debugger"
date:                  2024-01-26T03:50:38.186246-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att använda en debugger innebär att man nyttjar specialiserade verktyg för att kunna titta in under huven på sin kod, och se hur den körs steg för steg. Programmerare gör detta för att krossa buggar, optimera prestanda och förstå kodens beteende.

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