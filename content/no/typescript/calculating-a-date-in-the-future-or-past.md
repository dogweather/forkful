---
title:                "TypeScript: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å beregne en dato i fremtiden eller fortiden. Kanskje du planlegger en reise eller en viktig begivenhet, eller kanskje du bare er nysgjerrig på hvilken dag det var for et bestemt antall år siden. Å kunne beregne datoer kan være nyttig i mange ulike sammenhenger.

## Hvordan

Å beregne en dato i TypeScript er enkelt og kan gjøres ved hjelp av noen få linjer med kode. Først må du importere "date-fns" biblioteket ved å skrive følgende kode:

```TypeScript
import { addDays, subYears, format } from 'date-fns';
```

Deretter kan du bruke funksjoner som "addDays" og "subYears" for å legge til eller trekke fra et bestemt antall dager eller år fra en gitt dato. Du kan deretter formatere datoen med "format" funksjonen for å få ønsket format på datoen.

Her er et eksempel på hvordan du kan beregne en dato som er fem dager frem i tid og formatere den som DD/MM/YYYY:

```TypeScript
let nå = new Date();
let fremtidigDato = addDays(nå, 5);
let formatertDato = format(fremtidigDato, 'dd/MM/yyyy');
console.log(formatertDato);
```

Dette vil gi følgende output:

```
12/09/2021
```

## Dypdykk

Det finnes flere funksjoner i "date-fns" biblioteket som kan være nyttige når du skal beregne datoer. Du kan for eksempel også bruke funksjoner som "addMonths" og "subHours" for å legge til eller trekke fra måneder eller timer. Du kan også bruke "isBefore" funksjonen for å sjekke om en dato er før en annen dato.

Det er også mulig å beregne en dato basert på dag, måned og år i stedet for å bruke dagens dato. Dette kan gjøres ved å bruke "set" funksjonen som lar deg endre enkeltelementer av en dato. Her er et eksempel på hvordan du kan beregne en dato som er 100 år tilbake i tid:

```TypeScript
let nå = new Date();
let historiskDato = subYears(nå, 100);
historiskDato = set(historiskDato, { day: 1, month: 1, year: 1921 });
let formatertDato = format(historiskDato, 'dd/MM/yyyy');
console.log(formatertDato);
```

Dette vil gi følgende output:

```
01/01/1921
```

## Se også

- [date-fns dokumentasjon](https://date-fns.org/docs)
- [TypeScript offisiell nettside](https://www.typescriptlang.org/)
- [Enkel guide til å komme i gang med TypeScript](https://www.freecodecamp.org/news/hack-with-the-best-an-easy-guide-to-typescript/)