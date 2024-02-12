---
title:                "Konvertere en streng til små bokstaver"
aliases: - /no/javascript/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:46.173798-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle store bokstaver i teksten til deres små bokstav-ekvivalenter. Programmerere gjør dette for å forenkle sammenligning og behandling av tekst, for eksempel søk eller sortering.

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
