---
title:                "TypeScript: Sjekke om en mappe eksisterer."
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å sjekke om en mappe eksisterer er en viktig del av filbehandling i TypeScript. Dette kan hjelpe utviklere å unngå unødvendige feil og krasj i deres kode.

## Hvordan 

For å sjekke om en mappe eksisterer, kan vi bruke Node.js innebygd modul "fs". Dette gir oss funksjoner for å jobbe med filer og mapper.

```TypeScript
import * as fs from 'fs';

const mappeNavn = 'testMappe';

// Sjekker om mappen eksisterer 
if (fs.existsSync(mappeNavn)) {
    console.log('Mappen eksisterer');
} else {
    console.log('Mappen eksisterer ikke');
}
```

Eksempelet over viser hvordan vi kan bruke funksjonen "fs.existsSync()" for å sjekke om en mappe eksisterer. Hvis mappen finnes, vil konsollen skrive ut "Mappen eksisterer". Hvis mappen ikke finnes, vil den skrive ut "Mappen eksisterer ikke".

Et annet alternativ er å bruke "fs.accessSync()" funksjonen. Denne sjekker ikke bare om mappen eksisterer, men også om vi har tilgang til den.

```TypeScript
import * as fs from 'fs';

const mappeNavn = 'testMappe';

// Sjekker om mappen finnes og om vi har tilgang
try {
    fs.accessSync(mappeNavn, fs.constants.F_OK);
    console.log('Vi har tilgang til mappen');
} catch (err) {
    console.log('Vi har ikke tilgang til mappen');
}
```

I dette eksemplet bruker vi "fs.accessSync()" i en "try-catch" blokk. Hvis vi har tilgang til mappen, vil konsollen skrive ut "Vi har tilgang til mappen". Hvis vi ikke har tilgang, vil den skrive ut "Vi har ikke tilgang til mappen".

## Dypdykk

Når vi sjekker om en mappe eksisterer, kan vi også spesifisere hvilken type tilgang vi ønsker å sjekke. Dette gjøres ved å bruke konstanter fra "fs.constants" modulen.

For eksempel kan vi bruke "fs.constants.R_OK" for å sjekke om vi har lesetilgang til mappen, eller "fs.constants.W_OK" for å sjekke om vi har skrivetilgang.

```TypeScript
import * as fs from 'fs';

const mappeNavn = 'testMappe';

// Sjekker om mappen eksisterer og om vi har lese- og skrivetilgang
try {
    fs.accessSync(mappeNavn, fs.constants.R_OK | fs.constants.W_OK);
    console.log('Vi har både lese- og skrivetilgang til mappen');
} catch (err) {
    console.log('Vi har ikke både lese- og skrivetilgang til mappen');
}
```

Det er også verdt å merke seg at "fs.accessSync()" funksjonen vil returnere en feil hvis mappen ikke eksisterer. Dette kan være nyttig å vite hvis du ønsker å håndtere forskjellige tilfeller i koden din.

## Se også 

- [Offisiell Node.js dokumentasjon for "fs" modulen](https://nodejs.org/api/fs.html)
- [Guide for å lære TypeScript filbehandling](https://www.digitalocean.com/community/tutorials/typescript-filbehandling)
- [10 sbeste måter å sjekke om en fil eksisterer i JavaScript](https://insource.io/blog/articles/js-typescript/check-if-file-exists)