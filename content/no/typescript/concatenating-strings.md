---
title:                "Sammenslåing av strenger"
html_title:           "TypeScript: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

### Hva og hvorfor?
Sammenslåing av strenger er en vanlig teknikk som brukes av programmerere for å kombinere flere tekststrenger til en enkelt streng. Dette er nyttig for å lage dynamiske meldinger, konkatenere URLer eller formatere data. 

### Slik gjør du det:
```TypeScript
let fornavn = "Per";
let etternavn = "Olsen";
let navn = fornavn + etternavn;

console.log(navn); // Output: "PerOlsen"
```

### Utforsk dypere:
Konkatenasjon av strenger har blitt brukt av programmerere siden de tidlige dagene av programmeringsspråk. I TypeScript kan du bruke operator `+` eller metoden `concat()` for å slå sammen strenger. Alternativt kan du også bruke literale skjemaer for å enkelt lage dynamiske strenger.

### Se også:
- [Dokumentasjon for literale skjemaer i TypeScript](https://www.typescriptlang.org/docs/handbook/declaration-files/by-example.html#string-literal-types)
- [Eksempel på bruk av konkatenasjon av strenger i JavaScript](https://www.w3schools.com/js/js_string_concat.asp)