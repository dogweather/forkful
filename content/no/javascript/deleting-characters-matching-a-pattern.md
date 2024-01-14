---
title:                "Javascript: Slette tegn som matcher et mønster"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger kan det hende at vi trenger å fjerne deler av en tekst som samsvarer med et bestemt mønster. Dette kan være nyttig når man ønsker å rense opp i en tekst, for eksempel når man jobber med input fra brukere. Å kunne slette karakterer som matcher et mønster i Javascript kan være en verdifull evne for enhver utvikler.

## Hvordan gjøre det

For å slette karakterer som matcher et mømster i Javascript, kan vi bruke metoden `replace()` sammen med regulære uttrykk (regex). La oss se på et eksempel:

```Javascript
let tekst = "Hei, jeg heter Emma og jeg elsker Javascript";
let nyTekst = tekst.replace(/e+/g, "");
console.log(nyTekst);
```

Her bruker vi `replace()`-metoden sammen med regex `/e+/g` for å slette alle forekomster av én eller flere bokstaver "e" i teksten. Outputen vil være "Hi, jg htr Emma og jg lskr Javasript".

Det er også mulig å kombinere regex med andre string metoder, som for eksempel `match()` for å finne ut hvilke deler av teksten som matcher mønsteret.

## Dypdykk

Regex er en kraftig funksjon som kan brukes til å finne og manipulere tekst på en effektiv måte. Det finnes mange forskjellige mønstre som kan brukes i regex, og noen kan virke forvirrende og komplekse. Det er derfor viktig å øve seg og få en god forståelse av hvordan regex fungerer for å kunne utnytte det på best mulig måte.

Et annet viktig poeng å huske på er at regex er case-sensitive, det vil si at store og små bokstaver skiller mellom mønstrene. For å unngå dette kan man bruke flagget `i` etter regex, som vil gjøre søket case-insensitive.

## Se også

- [W3Schools - JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)
- [Regex Tutorial - A Quick Guide to Regular Expressions in Javascript](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)
- [MDN - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)