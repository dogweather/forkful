---
title:                "TypeScript: Uthenting av delstrenger"
simple_title:         "Uthenting av delstrenger"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut substrings, eller deler av en streng, er en vanlig oppgave i programmering. Dette kan være nyttig for å manipulere data eller hente ut spesifikke deler av en tekststreng. I denne blogginnlegget skal vi se på hvordan man kan gjøre dette i TypeScript.

## Slik gjør du det

For å trekke ut en substring i TypeScript, kan du bruke metoden `substring()` på en strengvariabel. Denne metoden tar inn to argumenter: startindeks og stoppindeks. La oss se på et eksempel:

```TypeScript
let tekst = "Dette er en tekststreng";

let delTekst = tekst.substring(5, 8);

console.log(delTekst);
```

Dette vil resultere i output-en `er`. Her har vi brukt metoden til å trekke ut deler fra den opprinnelige `tekst`-variabelen, fra og med indeks 5 til og med indeks 8.

Hvis du kun ønsker å trekke ut en del av en streng fra og med en bestemt indeks, kan du bare bruke én argument i `substring()`-metoden. For eksempel:

```TypeScript
let tekst = "Dette er en tekststreng";

let delTekst = tekst.substring(12);

console.log(delTekst);
```

Her vil `substring()`-metoden automatisk bruke indeksen 12 som start og trekke ut alt etter dette. Dette vil resultere i output-en `tekststreng`.

## Dykk dypere

I tillegg til `substring()`-metoden, kan man også bruke `slice()`-metoden for å trekke ut substrings i TypeScript. Denne metoden fungerer på samme måte som `substring()`, men tar inn negative indekser for å trekke ut deler fra slutten av strengen. Her er et eksempel:

```TypeScript
let tekst = "Dette er en tekststreng";

let delTekst = tekst.slice(-6);

console.log(delTekst);
```

Dette vil gi output-en `steng`, siden vi trekker ut de siste 6 bokstavene av strengen.

Det er også verdt å merke seg at `substring()` og `slice()` returnerer en ny streng og endrer ikke den opprinnelige strengen. Hvis du ønsker å endre den opprinnelige strengen, kan du bruke `replace()`-metoden.

## Se også

- [Dokumentasjon for `substring()`](https://www.tutorialspoint.com/typescript/string_substring.htm)
- [Dokumentasjon for `slice()`](https://www.tutorialspoint.com/typescript/string_slice.htm)
- [Dokumentasjon for `replace()`](https://www.tutorialspoint.com/typescript/string_replace.htm)