---
title:                "TypeScript: Utvinning av substrings"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut substrings er en vanlig oppgave når du jobber med tekstbehandling i programmering. Dette kan være nyttig for å manipulere og behandle data på en mer detaljert og presis måte. I denne blogginnlegget vil vi gå gjennom hvordan du kan trekke ut substrings ved hjelp av TypeScript.

## Slik gjør du det

For å trekke ut substrings i TypeScript bruker vi metoden `substring()` sammen med indeksene til start og slutt på den delen av teksten vi ønsker å hente ut. La oss se på et enkelt eksempel:

```TypeScript
let tekst = "Hei, dette er en test";

let substring = tekst.substring(5, 10);

console.log(substring);
```

Dette eksemplet vil gi oss følgende output:

```TypeScript
dette
```

I koden over har vi definert en tekststreng og deretter brukt `substring()`-metoden for å hente ut teksten fra indeks 5 til indeks 10. Det er viktig å huske på at indeksene starter på 0, så det betyr at vi egentlig henter ut teksten fra den sjette (5 + 1) til den ellevte (10 + 1) bokstaven i teksten.

Vi kan også bruke negative indekser i `substring()`-metoden. Dette vil gi oss teksten fra slutten av teksten, som vist i eksempelet under:

```TypeScript
let tekst = "Hei, dette er en test";

let substring = tekst.substring(-4, -1);

console.log(substring);
```

Outputen for dette eksemplet vil være:

```TypeScript
tes
```

Vi kan også bruke variabler eller konstanter som indekser, avhengig av hva som passer best for oppgaven vår. Her er et eksempel på hvordan vi kan bruke konstanter som indekser:

```TypeScript
let tekst = "Hei, dette er en test";

const START_INDEX = 5;
const END_INDEX = 10;

let substring = tekst.substring(START_INDEX, END_INDEX);

console.log(substring);
```

## Dykk dypere

Det er også verdt å nevne at `substring()`-metoden ikke endrer på den originale teksten, men heller returnerer en ny tekststreng basert på parametrene vi har gitt den. Hvis du ønsker å endre på den originale teksten, kan du bruke metoden `splice()` i stedet.

Det er også mulig å hente ut deler av en tekst basert på kun én indeks i `substring()`-metoden. Hvis vi kun gir en startindeks, vil metoden hente ut teksten fra dette punktet og helt til slutten av teksten. Og hvis vi kun gir en sluttindeks, vil metoden hente ut teksten fra begynnelsen av teksten og til dette punktet.

## Se også

- [MDN web docs: substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [TypeScript documentation: string methods](https://www.typescriptlang.org/docs/handbook/2/strings.html#string-methods)