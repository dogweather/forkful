---
aliases:
- /nl/javascript/rounding-numbers/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:55.521829-07:00
description: "Afronden is het weglaten van de 'ruis' na een bepaald punt in een nummer.\
  \ Programmeurs ronden af om precisie te beheersen, geheugen te beheren of de\u2026"
lastmod: 2024-02-18 23:09:02.270043
model: gpt-4-0125-preview
summary: "Afronden is het weglaten van de 'ruis' na een bepaald punt in een nummer.\
  \ Programmeurs ronden af om precisie te beheersen, geheugen te beheren of de\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?
Afronden is het weglaten van de 'ruis' na een bepaald punt in een nummer. Programmeurs ronden af om precisie te beheersen, geheugen te beheren of de output gebruiksvriendelijk te maken—zoals het omzetten van 2.998 in een schone 3.

## Hoe:
Hier is hoe je nummers afrondt in JavaScript met `Math.round()`, `Math.ceil()`, en `Math.floor()`:

```javascript
let origineelNummer = 2.567;

let naarBenedenAfgerond = Math.floor(origineelNummer); // 2
let naarBovenAfgerond = Math.ceil(origineelNummer);    // 3
let afgerond = Math.round(origineelNummer);            // 3 (aangezien .567 meer is dan .5)

console.log(naarBenedenAfgerond); // Print: 2
console.log(naarBovenAfgerond);   // Print: 3
console.log(afgerond);            // Print: 3
```

Om tot een bepaald aantal decimalen te fixeren, gebruik `toFixed()`:

```javascript
let tweeDecimalen = origineelNummer.toFixed(2); // "2.57" (geeft een string terug)

console.log(tweeDecimalen); // Print: "2.57"
```

Zet de string terug naar een nummer met een unair plusje of `Number()`:

```javascript
let weerEenNummer = +tweeDecimalen; // 2.57

console.log(weerEenNummer); // Print: 2.57
```

## Diepe Duik
Nummers afronden is niet nieuw; het is zo oud als de nummers zelf. In JavaScript gebruikt `Math.round()` de "ronde helft omhoog" tie-breaking: als het fractiegedeelte 0.5 is, wordt er afgerond naar het dichtstbijzijnde even nummer.

Voor meer controle is `toFixed()` misschien je beste optie, maar onthoud dat het een string teruggeeft. Terug converteren naar een nummer kan een extra stap zijn, maar zorgt ervoor dat je blijft werken met numerieke typen.

Alternatieven? Bibliotheken als `lodash` bieden `_.round(number, [precision=0])` voor meer genuanceerde controle. Of, de nieuwere `Intl.NumberFormat` geeft je hoogprecieze formatting, verder dan alleen afronden.

Over precisie gesproken, pas op voor de eigenaardigheden van drijvende-kommagetallen in JavaScript. `0.1 + 0.2` is niet exact gelijk aan `0.3` vanwege hoe nummers worden opgeslagen. Soms is afronden noodzakelijk om dergelijke drijvende-kommagetallenfouten te corrigeren.

## Zie Ook
- Mozilla's Math documentatie: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Financieel afronden met `Intl.NumberFormat`: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` afronden: [Lodash Docs](https://lodash.com/docs/4.17.15#round)
