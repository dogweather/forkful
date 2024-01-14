---
title:                "TypeScript: Henter nåværende dato"
simple_title:         "Henter nåværende dato"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få dagens dato er en viktig del av mange programmeringsprosjekter. Det kan være nyttig for å vise brukeren den nåværende datoen, eller for å håndtere tidssensitive data i et program. Det er også en god måte å lære om håndtering av datoer og tidsstyring i programmering.

## Hvordan

For å få dagens dato i TypeScript, kan du bruke den innebygde klassen "Date" og dens metoder. Her er et eksempel på hvordan du kan få dagens dato og skrive den ut til konsollen:

```TypeScript
const dato = new Date();
console.log(dato.toDateString());
```

Dette koden vil opprette en ny datoobjekt og deretter bruke "toDateString()" for å konvertere datoen til en mer lesbar streng og skrive den til konsollen. Output vil se ut som dette:

```
Tue Aug 11 2020
```

Du kan også bruke "get" metodene til å få spesifikke deler av datoen, for eksempel å få måneden som et tall:

```TypeScript
const dato = new Date();
const måned = dato.getMonth() + 1;
console.log(måned);
```

Dette vil returnere dagens måned som et tall mellom 1 og 12. Output vil se slik ut:

```
8
```

Det er også mulig å formatere datoen ved hjelp av biblioteker som "moment.js" eller "date-fns". Disse gir mer fleksibilitet og funksjonalitet for å arbeide med datoer.

## Deep Dive

Dagens dato blir vanligvis lagret som en "timestamp" i programmering, som er antall millisekunder som har gått siden 1. januar 1970. Dette er en standardenhet for å håndtere datoer og tidspunkter i de fleste programmeringsspråk.

I TypeScript kan du også opprette egendefinert "Date" objekt ved å angi et spesifikt år, måned og dag, og bruke metoder som "getHours()" og "getMinutes()" for å få nøyaktig tidspunkt. Det er også viktig å ta hensyn til tidssonen når man håndterer datoer, da det kan påvirke hva som faktisk er "dagens dato".

## Se Også

Her er noen nyttige ressurser for å lære mer om å få dagens dato i TypeScript:

-Offisiell dokumentasjon for "Date" klassen i TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#example-1
-Moment.js dokumentasjon: https://momentjs.com/
-Date-fns dokumentasjon: https://date-fns.org/

Det er også nyttig å utforske ulike måter å formatere og håndtere datoer basert på det aktuelle prosjektet og dets behov.