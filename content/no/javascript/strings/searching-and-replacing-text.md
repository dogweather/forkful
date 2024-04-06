---
date: 2024-01-20 17:58:16.756907-07:00
description: "How to: Bruk `replace()` for enkel s\xF8k-og-erstatt. Regul\xE6re uttrykk\
  \ (regex) gir mer kontroll."
lastmod: '2024-04-05T21:53:42.128701-06:00'
model: gpt-4-1106-preview
summary: "Bruk `replace()` for enkel s\xF8k-og-erstatt."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

## How to:
```Javascript
let tekst = "Katter er flinke, katter er søte.";
let endretTekst = tekst.replace(/katter/gi, "hunder");
console.log(endretTekst); // Output: Hunder er flinke, hunder er søte.
```
Bruk `replace()` for enkel søk-og-erstatt. Regulære uttrykk (regex) gir mer kontroll.

```Javascript
let historie = "I 1991 ble JavaScript laget. JavaScript ble oppdatert i 2015.";
let oppdaterHistorie = historie.replace(/\d{4}/g, (år) => {
  return år == "1991" ? "1995" : (parseInt(år) + 1).toString();
});
console.log(oppdaterHistorie); // Output: I 1995 ble JavaScript laget. JavaScript ble oppdatert i 2016.
```

## Deep Dive:
Søk-og-erstatt i JavaScript benytter gjerne `.replace()` metoden. Historisk sett kommer teknikkene for tekstmanipulasjon fra tidlige programmeringsspråk som Perl.

Det finnes andre metoder som `.replaceAll()` og biblioteker som Lodash, men `.replace()` med regex er fleksibelt. 

Når du bruker regulære uttrykk (regex), kan du spesifisere globale (`g`) søk, ignorere store/små bokstaver (`i`), og bruke callback-funksjoner for mer dynamiske erstatninger. Vær obs på tunge regex operasjoner – de kan senke ytelsen.

## See Also:
- MDN Web Docs om `.replace()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex tester og tutorials: https://regexr.com/
- Lodash bibliotek: https://lodash.com/
