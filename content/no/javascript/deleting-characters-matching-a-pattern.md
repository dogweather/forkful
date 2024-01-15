---
title:                "Slette tegn som matcher et mønster"
html_title:           "Javascript: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster kan være nyttig i tilfeller der man ønsker å fjerne uønsket tekst i en streng, som for eksempel ekstra mellomrom eller spesialtegn.

## Slik gjør du det

```Javascript
// Eksempel på å slette alle mellomrom fra en streng
let tekst = "Dette er en tekst med ekstra mellomrom.";
let renTekst = tekst.replace(/\s/g, "");
console.log(renTekst); // Detteerentekstmedekstramellomrom.

// Eksempel på å slette spesialtegn fra en tekststreng
let tekst = "Dette er en tekst med #spesialtegn.";
let renTekst = tekst.replace(/[#]/g, "");
console.log(renTekst); // Dette er en tekst med spesialtegn.
```

I de to eksemplene ovenfor bruker vi metoden .replace() for å erstatte alle mellomrom eller spesialtegn som matcher mønsteret vårt, med en tom streng. Vi bruker også regulære uttrykk (regex) ved å bruke \s for å finne alle mellomrom og [#] for å finne alle forekomster av spesialtegnet #. Det er også mulig å bruke andre regex-uttrykk for å matche forskjellige mønstre og deretter slette dem.

## Dypdykk

Når vi bruker metoden .replace() for å slette tegn som matcher et mønster, så må vi være oppmerksomme på at den bare sletter de første forekomstene av mønsteret. For å slette alle forekomster, må vi bruke en global flagg (/g) i vårt regex-uttrykk. Uten denne flaggen vil metoden bare slette den første forekomsten og stoppe der.

Det er også verdt å merke seg at metoden .replace() returnerer en ny tekststreng og endrer ikke den opprinnelige. Derfor må vi lagre denne nye strengen i en variabel for å bruke den videre.

## Se også

- [MDN: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN: Regular Expressions - Using regular expressions in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)