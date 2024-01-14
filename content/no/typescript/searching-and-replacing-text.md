---
title:                "TypeScript: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave for utviklere, enten det er for å endre variabelnavn, legge til funksjonalitet eller rette opp skrivefeil. Ved å bruke TypeScript kan du enkelt utføre denne oppgaven på en effektiv måte. I denne bloggposten vil vi forklare hvorfor det er viktig å kunne søke og erstatte tekst, og hvordan du kan gjøre det på en enkel måte.

## Hvordan

For å søke og erstatte tekst i TypeScript, kan du bruke metoden `replace()` som tar to parametere: søketeksten og erstatningsteksten. La oss si at du ønsker å bytte ut alle forekomster av ordet "hallo" med "hei" i en tekststreng:

```TypeScript
let tekst = "Hei, hvordan har du det? Hallo til deg også!";
let nyTekst = tekst.replace("hallo", "hei");
console.log(nyTekst);
```

Output:

```
Hei, hvordan har du det? Hei til deg også!
```

Som du kan se, erstattet `replace()` alle forekomster av "hallo" med "hei". Du kan også bruke regulære uttrykk for å søke og erstatte tekst i TypeScript, ved å gi en `RegExp` som første parameter. For eksempel, hvis du ønsker å bytte ut alle tall i en tekststreng med "X", kan du bruke følgende kode:

```TypeScript
let tallTekst = "1, 2, 3, 4, 5";
let nyTekst = tallTekst.replace(/\d/g, "X");
console.log(nyTekst);
```

Output:

```
X, X, X, X, X
```

Dette erstatter alle tall (representert av `\d` i regulære uttrykk) med "X". Du kan også bruke flere flagg sammen med regulære uttrykk, for eksempel `i` for å ignorere store og små bokstaver.

## Dypdykk

Når du bruker `replace()` i TypeScript, returnerer metoden en ny tekststreng og endrer ikke den opprinnelige. Dette kan være nyttig hvis du ønsker å beholde originalteksten og gjøre endringer i en kopi. Du kan også bruke `replace()` inne i en løkke for å erstatte forskjellige tekster basert på visse kriterier. Det er også viktig å være klar over at `replace()` bare erstatter den første forekomsten av teksten hvis du ikke bruker et regulært uttrykk med `g`-flagget.

## Se også

- [Grundig guide til regulære uttrykk i TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Eksempler på søk og erstatting i TypeScript](https://www.tutorialspoint.com/typescript/typescript_string_replace.htm)
- [Md5-kryptering med TypeScript](https://www.npmjs.com/package/md5-typescript)