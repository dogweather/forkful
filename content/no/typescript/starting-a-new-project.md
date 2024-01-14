---
title:    "TypeScript: Å starte et nytt prosjekt"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor starte et nytt prosjekt?

Å starte et nytt programmeringsprosjekt kan føles overveldende og utfordrende, men det kan også være veldig givende. Det å lage noe fra bunnen av, og se det utvikle seg til et ferdig produkt, kan være utrolig tilfredsstillende. Å starte et nytt prosjekt gir deg også mulighet til å utforske nye ideer og lære nye programmeringsspråk og verktøy.

## Hvordan starte et nytt prosjekt i TypeScript

For å starte et nytt prosjekt i TypeScript, må du først sørge for at du har installert TypeScript på datamaskinen din ved å følge disse trinnene:

1. Åpne terminalen og naviger til prosjektmappen din.
2. Skriv inn kommandoen `npm init` for å initialisere et nytt prosjekt.
3. Bruk deretter kommandoen `npm install typescript --save-dev` for å installere TypeScript-pakken.
4. Nå kan du opprette en `tsconfig.json`-fil ved hjelp av kommandoen `tsc --init`, som vil konfigurere TypeScript for prosjektet ditt.

Etter at du har satt opp prosjektet ditt, kan du begynne å skrive TypeScript-kode. Her er et eksempel på en enkel funksjon som legger sammen to tall og returnerer summen:

```typescript
function addNumbers(a: number, b: number): number {
  return a + b;
}
```

For å kjøre koden, må du først kompilere den til JavaScript ved hjelp av kommandoen `tsc`, og deretter kan du kjøre den ved å bruke `node`-kommandoen, for eksempel `node myFile.js`.

## Dypdykk i å starte et nytt prosjekt

Når du starter et nytt prosjekt, er det viktig å planlegge og organisere det godt. Her er noen tips som kan hjelpe deg å komme i gang:

- Tenk på hva slags prosjekt du vil lage og hva målet ditt er med det.
- Gjør litt research for å finne ut hvilke verktøy og teknologier som er best egnet for prosjektet ditt.
- Husk å ha en tydelig struktur og organisering av filer og mapper i prosjektet ditt.
- Fokuser på å skrive ren og effektiv kode for å unngå problemer senere i prosjektet.

Med disse tipsene i bakhodet, er du klar til å starte et spennende nytt prosjekt i TypeScript!

## Se også

- [Offisiell TypeScript dokumentasjon](https://www.typescriptlang.org/docs/)
- [TypeScript kurs på Udemy](https://www.udemy.com/course/understanding-typescript/)