---
title:                "TypeScript: Søking og erstatning av tekst"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig del av programmering, spesielt når man jobber med større mengder av kode. Dette gjøres for å effektivisere arbeidsprosessen og sikre at kodebasen er konsistent og feilfri.

## Hvordan

Det finnes flere forskjellige måter å søke og erstatte tekst på i TypeScript. Her er et enkelt eksempel på hvordan man kan bruke metoden `replace` for å erstatte all forekomst av et spesifikt ord i en tekststreng:

```TypeScript
let tekst = "Hei alle sammen! Jeg heter Maria og jeg er veldig glad i å programmere."

// Erstatter "Maria" med "Markus"
let erstattetTekst = tekst.replace("Maria", "Markus");
console.log(erstattetTekst);

// Output: Hei alle sammen! Jeg heter Markus og jeg er veldig glad i å programmere.
```

I dette eksempelet ser vi at metoden `replace` tar inn to argumenter, det første er teksten som skal erstattes, mens det andre er teksten som skal erstatte den originale teksten. Slik kan man enkelt og effektivt endre tekst i en variabel.

Det er også mulig å bruke såkalte regulære uttrykk i TypeScript for å gjøre mer avanserte søk og erstattninger. Her er et eksempel på en regex som erstatter alle tall i en tekststreng med ordet "nummer":

```TypeScript
let tallTekst = "Dette er tekst med tall: 123 456 789";

// Regex som erstatter tall med ordet "nummer"
let nyTekst = tallTekst.replace(/[0-9]+/g, "nummer");
console.log(nyTekst);

// Output: Dette er tekst med tall: nummer nummer nummer
```

Med regex kan man utnytte jokertegn og spesifikke kriterier for å søke og erstatte mer presist.

## Dypdykk

Det er viktig å være oppmerksom på at metoden `replace` i TypeScript bare erstatter den første forekomsten av teksten i en streng, med mindre man bruker regulære uttrykk med flagget "g" for globalt søk.

Det finnes også flere string-metoder i TypeScript som kan være nyttige i søke- og erstattingsprosessen, som for eksempel `split` og `substr`.

Det er også verdt å nevne at TypeScript har full støtte for Unicode, så man kan søke og erstatte teksten uavhengig av hvilket språk eller tegnsett det er skrevet på.

## Se også

- [String.prototype.replace - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [RegExp - TypeScript Deep Dive](https://basarat.gitbook.io/typescript/type-system/regular-expression)
- [String - TypeScript Docs](https://www.typescriptlang.org/docs/handbook/strings.html)