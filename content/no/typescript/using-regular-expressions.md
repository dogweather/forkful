---
title:                "Å bruke regulære uttrykk"
html_title:           "TypeScript: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Bruk av regulære uttrykk (også kjent som regex) er en måte å søke og manipulere tekst på i programmering. Det er mye brukt av utviklere for å finne og utskifte deler av tekst eller for å validere tekstformater. Regex er en kraftig verktøy som kan gjøre tekstbehandling enklere og mer effektivt.

## Hvordan:
Bruk av regex i TypeScript er enkelt. I følgende eksempel viser vi hvordan vi kan finne alle tall i en tekst og skrive dem ut:

```TypeScript
let tekst = "Jeg har 3 epler og 5 bananer.";
let tall = tekst.match(/\d+/g);
console.log(tall);
```
Output vil være `[ '3', '5' ]` som innebærer at vi har funnet og fanget to tall fra teksten.

## Dypdykk:
Regulære uttrykk har eksistert siden 1950-tallet og blir brukt i flere programmeringsspråk. I tillegg til TypeScript, kan du også bruke regex i språk som Python og Perl. Alternativene til regex inkluderer stringmetoder som søking og erstattning, men disse er ikke like kraftige som regex og kan bli mer komplisert når man håndterer komplekse mønstre. Implementeringen av regex i TypeScript er basert på standarden for regulære uttrykk fra ECMAScript.

## Se også:
- [W3Schools tutorial on Regular Expressions in TypeScript (Engelsk)](https://www.w3schools.com/jsref/jsref_obj_regex.asp)
- [Official TypeScript Documentation - Regular Expressions (Engelsk)](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Introduksjon til regulære uttrykk for norske programmerere (Norsk)](https://www.dagensit.no/viewtopic.php?f=16&t=1882)