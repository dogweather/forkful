---
title:                "Bruke associative tabeller"
aliases:
- no/typescript/using-associative-arrays.md
date:                  2024-01-30T19:13:36.962345-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke associative tabeller"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative arrays, eller objekter i TypeScript, lar deg bruke strenger (eller nøkler) for å få tilgang til verdi-par. Programmerere bruker dem for mer dynamiske datatilgangsmønstre sammenlignet med tradisjonelle arrays, noe som gir en fleksibel måte å strukturere og få tilgang til data på uten å være knyttet til numeriske indekser.

## Hvordan:

Å opprette og bruke assosiative arrays i TypeScript er enkelt. Her er en grunnleggende gjennomgang:

```TypeScript
// Deklarerer et assosiativt array
let bruker: { [key: string]: string } = {};

// Legger til data
bruker["navn"] = "Ola Nordmann";
bruker["epost"] = "ola@example.com";

console.log(bruker);
```

Utdata:

```TypeScript
{ navn: 'Ola Nordmann', epost: 'ola@example.com' }
```

Det er også enkelt å iterere over nøkkel-verdi-par:

```TypeScript
for (let nøkkel in bruker) {
    console.log(nøkkel + ": " + bruker[nøkkel]);
}
```

Utdata:

```TypeScript
navn: Ola Nordmann
epost: ola@example.com
```

Og hvis du har med en blanding av datatyper å gjøre, kommer TypeScript sin typesystem godt med:

```TypeScript
let blandeteTyper: { [key: string]: string | number } = {};
blandeteTyper["navn"] = "Kari Nordmann";
blandeteTyper["alder"] = 30;

console.log(blandeteTyper);
```

Utdata:

```TypeScript
{ navn: 'Kari Nordmann', alder: 30 }
```

## Dypdykk

I TypeScript er det vi refererer til som assosiative arrays egentlig objekter. Historisk sett, i språk som PHP, er assosiative arrays en grunnleggende type, men JavaScript (og ved forlengelse, TypeScript) bruker objekter for dette formålet. Denne tilnærmingen er både en styrke og en begrensning. Objekter gir en svært dynamisk struktur for å assosiere strenger til verdier, men de er ikke ment å bli brukt som 'arrays' i tradisjonell forstand. For eksempel kan du ikke bruke array-metoder som `push` eller `pop` direkte på disse objektene.

For tilfeller hvor du trenger ordnede samlinger av nøkkel-verdi-par med array-lignende operasjoner, tilbyr TypeScript (og moderne JavaScript) `Map`-objektet:

```TypeScript
let brukerMap = new Map<string, string>();
brukerMap.set("navn", "Ola Nordmann");
brukerMap.set("epost", "ola@example.com");

brukerMap.forEach((verdi, nøkkel) => {
    console.log(nøkkel + ": " + verdi);
});
```

Selv om TypeScript sitt typesystem og ES6-funksjoner som `Map` tilbyr kraftige alternativer, er forståelsen av hvordan man bruker objekter som assosiative arrays nyttig for scenarier hvor objekt-litteraler er mer effektive eller når du jobber med JSON-datastrukturer. Det handler alt om å velge det riktige verktøyet for jobben.
