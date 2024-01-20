---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulære uttrykk, eller regex, er søkemønstre for tekstmanipulasjon. De brukes fordi de kan raskt finne, erstatte, eller validere kompleks tekstdata.

## How to:
```TypeScript
// Finn alle forekomster av "eple"
const text = "Eplekake og eplejuice er fantastisk.";
const regex = /eple/gi;
console.log(text.match(regex));
// Output: [ 'Eple', 'eple' ]

// Erstatt "eple" med "banan"
console.log(text.replace(regex, 'banan'));
// Output: Banankake og bananjuice er fantastisk.

// Sjekk om en string er en gyldig e-postadresse
const emailRegex = /^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6}$/;
const email = "eksempel@domene.no";
console.log(emailRegex.test(email));
// Output: true
```

## Deep Dive
Regex eksisterte lenge før TypeScript, fra 1950-tallet. Alternativer inkluderer string-matching biblioteker eller innebyggede funksjoner som `startsWith`, `endsWith`, og `includes`. Ved å bruke regex i TypeScript, utføres tekstmanipulasjon direkte ved hjelp av JavaScript-motoren, noe som gir høy ytelse.

## See Also
- [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex Tester](https://regexr.com/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)