---
title:    "TypeScript: Sammenkobling av strenger"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

I programmering er det ofte behov for å kombinere flere tekststrenger for å lage en mer komplett og meningsfull tekst. Dette kalles å "konkatenere" strenger, og det er en nyttig teknikk som kan brukes i ulike situasjoner. I denne bloggposten skal vi se på hvordan man kan gjøre dette ved hjelp av TypeScript.

## Hvordan

For å konkatenere strenger i TypeScript kan du bruke operatøren "+" eller metoden `concat()`. La oss se på noen eksempler:

```TypeScript
// Definerer to variabler med tekst
let navn: string = "Ingrid";
let yrke: string = "programmerer";

// Kombinerer de to tekstene med +
let setning1: string = navn + " er en dyktig " + yrke + ".";

// Kombinerer de to tekstene med concat()
let setning2: string = navn.concat(" jobber som ", yrke, ".");

// Output: Ingrid er en dyktig programmerer.
console.log(setning1);
// Output: Ingrid jobber som programmerer.
console.log(setning2);
```

Som du kan se kan du bruke den "+" operatøren til å sette sammen flere deler av en tekststreng, og metoden `concat()` fungerer på samme måte, men tar inn flere argumenter istedenfor å bruke `+`.

## Dypdykk

Det er viktig å være oppmerksom på at når du konkatenere strenger med `+` operatøren, må du være sikker på at alle verdiene er av riktig type. Dette er spesielt viktig når du kombinerer tall og strenger. Hvis det er et tall, vil det automatisk bli konvertert til en streng før konkatenasjonen skjer, så du må være forsiktig med hvordan du bruker denne operatøren.

En annen ting å være oppmerksom på er at begge metodene, `+` og `concat()`, returnerer en helt ny streng og endrer ikke på de opprinnelige verdiene. Derfor er det viktig å lagre resultatet i en ny variabel hvis du ønsker å bruke den videre.

## Se også

- [https://www.typescriptlang.org/docs/handbook/basic-types.html#string](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [https://www.w3schools.com/jsref/jsref_concat_string.asp](https://www.w3schools.com/jsref/jsref_concat_string.asp)