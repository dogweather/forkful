---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regular expressions (regex) hjelper oss med å søke og manipulere tekst ved å definere et søkemønster. De er uunnværlige for tekstbehandling, validering og parsing fordi de sparer tid og kode.

## Hvordan:
```Javascript
// Sjekke om strengen inneholder et telefonnummer i format: 123-456-7890
let telefonRegex = /\d{3}-\d{3}-\d{4}/;
console.log(telefonRegex.test('Ring meg på 123-456-7890.')); // Output: true

// Erstatte små bokstaver med store bokstaver
let tekst = 'hei på deg!';
let erstattRegex = /[a-z]/g;
console.log(tekst.replace(erstattRegex, letter => letter.toUpperCase())); // Output: 'HEI PÅ DEG!'

// Trekke ut alle ord
let ordRegex = /\w+/g;
let minTekst = 'Regex er kult!';
console.log(minTekst.match(ordRegex)); // Output: ['Regex', 'er', 'kult']
```

## Dypdykk:
Regular expressions stammer fra 1950-tallets arbeid med formalisert språkteori og automatateori. Alternativer til regex inkluderer string metoder som `.indexOf()` og `.includes()`, men de er mindre kraftfulle. Ved implementering bør man være oppmerksom på 'greedy' vs 'non-greedy' matching og potensielle ytelsesproblemer ved komplekse mønstre.

## Se Også:
- [MDN Regular Expressions Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex101: Online regex tester og debugger](https://regex101.com/)
- [ECMAScript 2018 Language Specification](https://tc39.es/ecma262/#sec-regexp-regular-expression-objects)
