---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:10.503262-07:00
description: "Interactieve shells, of REPLs (Read-Eval-Print Loops), laten je on-the-fly\
  \ code uitvoeren, functies en algoritmen testen of spelen met idee\xEBn. Ze zijn\
  \ de\u2026"
lastmod: '2024-03-13T22:44:51.203102-06:00'
model: gpt-4-0125-preview
summary: "Interactieve shells, of REPLs (Read-Eval-Print Loops), laten je on-the-fly\
  \ code uitvoeren, functies en algoritmen testen of spelen met idee\xEBn."
title: Het gebruik van een interactieve shell (REPL)
weight: 34
---

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
