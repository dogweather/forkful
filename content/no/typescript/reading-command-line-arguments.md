---
title:    "TypeScript: Å lese kommandolinjeargumenter"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noensinne har programmert i TypeScript før, vet du at det er en kraftig og fleksibel programmeringsspråk. En av de mange nyttige funksjonene i TypeScript er evnen til å lese kommandolinjeargumenter. Dette gjør det mulig for programmerere å interagere med sine programmer på en enkel og effektiv måte. I denne bloggposten vil vi utforske hvorfor det er viktig å kunne lese kommandolinjeargumenter og hvordan du kan gjøre det i TypeScript.

## Hvordan gjøre det

Lesing av kommandolinjeargumenter i TypeScript er enkelt og kan gjøres ved hjelp av en innebygd Node.js modul kalt "process". Her er et eksempel på hvordan du kan lese og skrive ut en kommandolinjeargument i TypeScript:

```TypeScript
// Importer process modul
import * as process from 'process';

// Lagre kommandolinjeargumenter i et array
let args: string[] = process.argv;

// Skriv ut det første argumentet
console.log(`Første kommandolinjeargument: ${args[0]}`);
```

Når du kjører denne koden i terminalen og legger til et argument etter filnavnet, vil det første argumentet bli skrevet ut.

```bash
ts-node index.ts argument1
```

Output:
```
Første kommandolinjeargument: argument1
```

Du kan også bruke "slice" metoden for å få et subsett av argumentene. For eksempel, hvis du vil ha alle argumentene bortsett fra det første (som i eksemplet ovenfor), kan du bruke følgende kode:

```TypeScript
let subsetArgs: string[] = process.argv.slice(2);
```

Dette vil lagre alle kommandolinjeargumentene bortsett fra det første i variabelen "subsetArgs".

## Dykk litt dypere

Når du leser kommandolinjeargumenter i TypeScript, er det noen ting du bør være klar over. Først og fremst er det viktig å huske at argumentene alltid vil bli lagret som strenger, selv om du skriver inn tall. Derfor må du konvertere dem til riktig datatype hvis du trenger å bruke dem som noe annet enn en streng.

En annen viktig ting å huske på er at argumentene ikke vil bli inkludert hvis du kjører programmet ditt fra en integrert utviklingsmiljø (IDE) som Visual Studio Code. For å få argumentene til å vises må du kjøre programmet ditt fra terminalen eller kommandolinjen.

En god praksis når du leser kommandolinjeargumenter er å sjekke om brukeren har inkludert de riktige argumentene før du bruker dem. Dette kan gjøres ved hjelp av "length" metoden på "process.argv" arrayet.

```TypeScript
// Sjekk om det er minst to argumenter
if (process.argv.length < 3) {
    console.log("Feil: manglende argumenter, vennligst inkluder to argumenter");
} else {
    // Fortsett programmet
}
```

## Se også

- [Node.js dokumentasjon om "process" modulen](https://nodejs.org/api/process.html)
- [TypeScript dokumentasjon om kommandolinjeargumenter](https://www.typescriptlang.org/docs/handbook/interfaces.html)
- [Tutorialspoint artikkel om lesing av kommandolinjeargumenter i TypeScript](https://www.tutorialspoint.com/typescript/typescript_command_line.htm)