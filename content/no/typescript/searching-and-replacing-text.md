---
title:    "TypeScript: Søke og erstatte tekst"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave i programmering. Det kan hjelpe deg med å gjøre store endringer eller finne og fikse feil i koden din. Det kan også være nyttig når du må oversette en del av koden til et annet språk.

## Slik gjør du det

For å søke og erstatte tekst i TypeScript, kan du bruke den innebygde metoden `.replace()` på en streng. Denne metoden tar inn to argumenter: det gamle tekstsøket og det nye tekstsøket.

```TypeScript
let tekst = "Hei, verden!";
console.log(tekst.replace("Hei", "Hallo"));
```

Dette vil gi outputen: "Hallo, verden!". Som du kan se, erstattet `.replace()`-metoden "Hei" med "Hallo". En annen nyttig metode for å søke etter tekst er `.indexOf()`, som returnerer indeksen til den første forekomsten av teksten du søker etter.

## Dykk dypere

I tillegg til å erstatte tekst, kan du også bruke `.replace()`-metoden til å utføre Regex (regular expressions) søk og erstatninger. Dette gjør det mulig å søke og erstatte tekst basert på mønstre i stedet for nøyaktige tekstsøk.

For å utføre et Regex-søk, må du først opprette et Regex-objekt ved hjelp av `new RegExp()`. Deretter kan du bruke dette objektet sammen med `.replace()`-metoden til å søke og erstatte tekst.

```TypeScript
let tekst = "2020 var et vanskelig år, men 2021 blir bedre!";
let regex = new RegExp("2020", "g");
console.log(tekst.replace(regex, "2021"));
```

Dette vil gi outputen: "2021 var et vanskelig år, men 2021 blir bedre!". Som du kan se, erstattet `.replace()`-metoden alle forekomster av "2020" med "2021" ved hjelp av Regex-søket.

## Se også

- [MDN - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN - String.prototype.indexOf()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)
- [MDN - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)