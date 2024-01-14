---
title:                "TypeScript: Konvertering av dato til en streng"
simple_title:         "Konvertering av dato til en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor
Et av de vanligste prosessene i programmering er å konvertere data fra en type til en annen. Å konvertere en dato til en streng er spesielt nyttig for å vise datoen på en lesbar måte, for eksempel på et brukergrensesnitt eller i en logg. I denne bloggposten vil vi gå gjennom hvordan du kan konvertere en dato til en streng ved hjelp av TypeScript.

# Hvordan
For å konvertere en dato til en streng i TypeScript, bruker vi metoden `toString()`. La oss ta en enkel dato som eksempel og konvertere den til en streng:

```TypeScript
let date = new Date(2021, 7, 20); // Oppretter en dato for 20. august 2021
console.log(date.toString()); // Output: Wed Aug 20 2021 00:00:00 GMT+0200 (Central European Summer Time)
```

Vi kan også endre formatet på datoen ved å bruke metoden `toLocaleDateString()` og spesifisere hvilket språk vi ønsker datoen på. Se eksempelet nedenfor:

```TypeScript
console.log(date.toLocaleDateString('nb-NO')); // Output: 20.08.2021
```

Ved å bruke `toLocaleDateString()` kan vi også spesifisere ønsket format for datoen. Dette kan gjøres ved hjelp av en såkalt formatstring, som består av forskjellige symboler som representerer ulike deler av datoen. For eksempel kan du bruke `dd/MM/yy` for å få datoen i formatet dag/måned/år. Se eksempelet nedenfor:

```TypeScript
console.log(date.toLocaleDateString('nb-NO', { format: "dd/MM/yy" })); // Output: 20/08/21
```

# Dypdykk
Når vi bruker `toLocaleDateString()` i TypeScript, bruker den standard språkinnstillingene på datamaskinen. Dette kan resultere i at datoen vises på et annet språk enn det som er ønsket. For å løse dette problemet kan vi bruke tredjeparts pakker som `date-fns` eller `moment.js` som gir oss mer kontroll over konverteringen og formatet på datoen. Disse pakkene lar oss også enkelt oversette datoen til ønsket språk.

# Se også
- [date-fns dokumentasjon](https://date-fns.org/v2.23.0/docs/format)
- [moment.js dokumentasjon](https://momentjs.com/docs/#/displaying/format/)