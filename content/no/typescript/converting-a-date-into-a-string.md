---
title:                "TypeScript: Konvertering av en dato til en streng"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Konvertering av en dato til en streng er en vanlig oppgave ved utvikling av programmer, spesielt i webapplikasjoner der datoer vises for brukere. Det er viktig å kunne vise datoer på en forståelig måte for brukerne, og dette kan oppnås ved å konvertere datoen til en streng.

## Hvordan gjøre det
For å konvertere en dato til en streng i TypeScript, kan vi bruke den innebygde funksjonen `toString()`. Dette er en standardfunksjon i de fleste programmeringsspråk som konverterer en dato til en lesbar streng ved å følge et visst format. La oss se på et eksempel:

```TypeScript
const date = new Date(); // Oppretter en ny dato
const dateString = date.toString(); // Konverterer datoen til en streng
console.log(dateString); // Output: Sun Oct 03 2021 00:00:00 GMT+0200 (Central European Summer Time)
```

Som du kan se, følger utgangen et standardformat med navnet på ukedagen, måneden, dagen, året og tidsinformasjon. Dette kan variere avhengig av dine lokale innstillinger.

Hvis du vil ha mer kontroll over formatet på strengen, kan du bruke `toLocaleString()` -funksjonen. Denne funksjonen tar imot parametere for å spesifisere hvilket språk og region du ønsker datoen konvertert til, samt et format eller en stil for datoen. La oss se på et eksempel:

```TypeScript
const date = new Date(); // Oppretter en ny dato
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' }; // Angir formatet for datoen
const dateString = date.toLocaleString('nb-NO', options); // Konverterer datoen til en streng på norsk
console.log(dateString); // Output: søndag 3. oktober 2021
```

Som du kan se, kan vi angi et spesifikt språk (i dette tilfellet norsk) og et format for datoen. På denne måten kan vi tilpasse utgangen for å passe til våre behov og lokalisering.

## Dypdykk
Det er viktig å være klar over at når du konverterer en dato til en streng, mister du den opprinnelige datotypen. Dette betyr at du ikke lenger kan bruke metoder som er tilgjengelige for Date-objekter, for eksempel `getDay()` eller `getFullYear()`. Derfor bør du være forsiktig når du konverterer datoen til en streng og sørge for å lagre den opprinnelige datoen et annet sted hvis du trenger å bruke den senere.

## Se også
- [MDN webdocs: Date.prototype.toString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [MDN webdocs: Date.prototype.toLocaleString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)