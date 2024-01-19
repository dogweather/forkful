---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Lua: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Sjekke om en Mappe Eksisterer i TypeScript

## Hva og Hvorfor?

Å sjekke om en mappe eksisterer i programmering er akkurat hva det høres ut som: Vi sjekker om en bestemt mappe finnes i systemet. Programmører gjør dette for å unngå feil som oppstår når de forsøker å lese eller skrive fra en mappe som ikke finnes.

## Hvordan?

I TypeScript kan du sjekke om en mappe eksisterer ved hjelp av Node.js's `fs` (files system) bibliotek. Her er hvordan:

```TypeScript
import fs from 'fs';

if(fs.existsSync('/sti/til/mappen')) {
    console.log('Mappen eksisterer!');
} else {
    console.log('Mappen eksisterer ikke!');
}
```

Utdata ditt vil være en av disse to beskjedene, avhengig av om mappen eksisterer:

```TypeScript
'Mappen eksisterer!'
```
eller

```TypeScript
'Mappen eksisterer ikke!'
```

## Dypdykk

Å sjekke om en mappe eksisterer er ikke en ny oppfunnet metode; det har vært rundt så lenge det har vært filsystemer å lese og skrive til.

Hvis du bruker annen JavaScript-rammeverk enn Node.js, kan det hende du må bruke en annen metode for å sjekke om en mappe eksisterer. For eksempel, i en nettleser, har du ikke direkte tilgang til filsystemet.

I tillegg, Node `fs.existSync` metode er synkron. Det betyr at JavaScript vil vente til det har fullført før det fortsetter med neste linje i koden din. Selv om synkrone funksjoner kan være lettere å håndtere, kan de gjøre programmet ditt tregere hvis de tar lang tid.

## Se Også

- Node.js fs.exists: https://nodejs.org/api/fs.html#fs_fs_exists_path_callback
- Stack Overflow - How do you check if a directory exists in TypeScript: https://stackoverflow.com/questions/45584364/how-do-you-check-if-a-directory-exists-in-typescript.