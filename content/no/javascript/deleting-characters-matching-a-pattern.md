---
title:                "Javascript: Sletting av karakterer som matcher et mønster"
simple_title:         "Sletting av karakterer som matcher et mønster"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hvorfor slette tegn som matcher et mønster?

Å slette tegn som matcher et bestemt mønster er en vanlig oppgave for mange utviklere i Javascript. Dette kan være nyttig når man ønsker å fjerne uønskede eller feilaktige tegn fra en tekststreng, eller når man trenger å filtrere ut data basert på et bestemt mønster. Ved å lære denne teknikken, kan du forbedre effektiviteten og funksjonaliteten til koden din.

## Hvordan gjøre det

For å slette tegn som matcher et mønster, kan du bruke metoden `replace()` i Javascript. Denne metoden tar to parametere: det første er det mønsteret du ønsker å matche, og det andre er hva du ønsker å erstatte det mønsteret med. La oss se på et eksempel:

```Javascript
let tekst = "Hei! Dette er en tekst med tall: 12345";
let pattern = /\d/g;
let nyTekst = tekst.replace(pattern, '');
console.log(nyTekst); // Resultat: Hei! Dette er en tekst med tall:
```

I dette eksempelet bruker vi metoden `replace()` sammen med det regulære uttrykket `\d`, som matcher alle tall i teksten. Vi erstatter disse tallene med en tom streng, og får dermed en tekst uten tall.

## Dykk dypere

Det regulære uttrykket `\d` referer til en hvilken som helst tallverdi i teksten. Men hva om du ønsker å matche et bestemt tall eller en kombinasjon av tall? Da kan du bruke spesifikke tallverdier i uttrykket, som for eksempel `\d{4}`, som vil matche kun tall med fire siffer.

Du kan også kombinere flere uttrykk, for eksempel ved å matche både tall og bokstaver. Et eksempel på dette kan være `/[\dA-Za-z]/`, som vil matche både tall og alle bokstaver, uansett om de er store eller små.

## Se også

- [Javascript Documentation - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/javascript) 
- [W3Schools - Javascript Regular Expressions](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)