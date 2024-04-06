---
date: 2024-01-20 17:38:46.173798-07:00
description: "Hvordan: Tidligere m\xE5tte programmerere kanskje h\xE5ndtere omregning\
  \ av tekst manuelt, men JavaScript har lenge hatt innebygde metoder for \xE5 gj\xF8\
  re dette\u2026"
lastmod: '2024-04-05T21:53:42.130484-06:00'
model: gpt-4-1106-preview
summary: "Tidligere m\xE5tte programmerere kanskje h\xE5ndtere omregning av tekst\
  \ manuelt, men JavaScript har lenge hatt innebygde metoder for \xE5 gj\xF8re dette\
  \ enkelt."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

## Hvordan:
```javascript
let hilsen = "Hei, Verden!";
let litenHilsen = hilsen.toLowerCase();

console.log(litenHilsen);  // Output: "hei, verden!"
```

## Dypdykk:
Tidligere måtte programmerere kanskje håndtere omregning av tekst manuelt, men JavaScript har lenge hatt innebygde metoder for å gjøre dette enkelt. Metoden `toLowerCase()` er ikke den eneste—det finnes også `toUpperCase()`, for å gjøre om til store bokstaver, og med ECMAScript 2015 (ES6) kan vi bruke `localeCompare()` for å sammenligne strenger på en lokalisert måte.

Når det gjelder implementering, bruker `toLowerCase()` Unicode-verdier for å finne småbokstav-ekvivalenter. Ikke alle språk er like enkle—noen skriftsystemer har ikke klart skille mellom store og små bokstaver. Likevel for de fleste skriptene og språkene vi bruker på weben, gjør `toLowerCase()` jobben fint.

## Se Også:
- MDN Web Docs om `String.prototype.toLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- En introduksjon til Unicode i JavaScript: https://javascript.info/string#unicode
- ECMAScript 2015 (ES6) and nyere versjoner: https://www.ecma-international.org/ecma-262/6.0/
