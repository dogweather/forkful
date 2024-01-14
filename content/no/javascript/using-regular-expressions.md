---
title:                "Javascript: Å bruke regulære uttrykk"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor
Regulære uttrykk er et kraftig verktøy for å søke og manipulere tekst i programmering. Ved hjelp av mønstre og symboler kan du søke og erstatte tekst i en mer effektiv og fleksibel måte enn du ville være i stand til å gjøre manuelt. Hvis du jobber med tekstbaserte data eller brukergrensesnitt, er kunnskap om regulære uttrykk en essensiell ferdighet å ha i verktøykassen din.

## Hvordan
Å lage et regulært uttrykk i Javascript er enkelt. Du bruker bare en liten brikke av kode for å definere et mønster og de nødvendige symbolene for å søke og erstatte tekst. For eksempel, hvis du ønsker å finne alle forekomster av ordet "hallo" i en streng, kan du bruke følgende uttrykk:

```Javascript
/hallo/g
```

Outputen for dette uttrykket vil returnere en liste med alle forekomster av "hallo" i strengen.

## Dypdykk
I tillegg til det grunnleggende, er det flere avanserte bruksområder for regulære uttrykk som kan forbedre effektiviteten og nøyaktigheten til koden din. For eksempel kan du bruke forespørselen "i" for å gjøre søket ditt case-insensitive, eller du kan bruke parenteser for å gruppere mønstrene dine. Det finnes også en rekke forskjellige symboler for å søke etter spesifikke mønstre som tall, bokstaver og spesifikke ord.

## Se også
- [MDN Web Docs: Regulære uttrykk i JS](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex Tutorial](https://regexone.com)
- [Javascript Regulære Uttrykk Cheat Sheet](https://www.shortcutfoo.com/app/dojos/javascript-regex/cheatsheet)