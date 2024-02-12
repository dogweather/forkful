---
title:                "Het gebruik van een interactieve shell (REPL)"
aliases:
- /nl/javascript/using-an-interactive-shell-repl/
date:                  2024-01-28T22:09:10.503262-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Interactieve shells, of REPLs (Read-Eval-Print Loops), laten je on-the-fly code uitvoeren, functies en algoritmen testen of spelen met ideeën. Ze zijn de kladblokken van het programmeren, snel en vuil, zonder een volledige ontwikkelomgeving op te zetten.

## Hoe te:
Node.js wordt geleverd met een REPL die toegankelijk is via de terminal. Open het, en je bent klaar om te beginnen. Hier is een voorproefje:

```javascript
$ node
> laat sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

Eenvoudig, toch? Definieer variabelen, functies of voer lussen uit. Wanneer je klaar bent, brengt `.exit` je terug naar de echte wereld.

## Diepe Duik
REPLs bestaan al sinds de jaren 60 - LISP pionierde het concept. Het idee: geef onmiddellijke feedback aan de programmeur. Alternatieven? Naast Node.js REPL zijn er browser-gebaseerde consoles zoals Chrome DevTools, online zandbakken zoals JSFiddle, of volledige IDE's zoals VSCode met interactieve speeltuinen.

Onder de motorkap doorlopen REPL-workflows typisch:
1. Lees invoer
2. Compileer en voer code uit
3. Print uitvoer
4. Keer terug naar begin

Het is een eenvoudige maar effectieve cyclus die interactief coderen enorm heeft beïnvloed.

## Zie ook
- [Node.js REPL-documentatie](https://nodejs.org/api/repl.html)
- [Mozilla's Inleiding tot JavaScript-modules op REPL's](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
