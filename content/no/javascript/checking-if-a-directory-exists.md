---
title:                "Å sjekke om en mappe eksisterer"
html_title:           "Javascript: Å sjekke om en mappe eksisterer"
simple_title:         "Å sjekke om en mappe eksisterer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sjekke om en mappe eksisterer er en viktig del av programmering. Det kan hjelpe deg med å validere brukerinput, håndtere feil og forbedre brukeropplevelsen i applikasjonen din.

## Hvordan
Sjekke om en mappe eksisterer i Javascript er enkelt. Du kan bruke `fs.existsSync()` -funksjonen fra Node.js FileSystem-modulen. Denne funksjonen tar inn en sti som parameter og returnerer `true` hvis mappen eksisterer og `false` hvis den ikke gjør det.

```Javascript
// Importerer FileSystem-modulen
const fs = require('fs');

// Definerer mappen som skal sjekkes
const mappesti = "/bruker/dokumenter/";

// Sjekker om mappen eksisterer
if(fs.existsSync(mappesti)){
    console.log("Mappen eksisterer!");
} else{
    console.log("Mappen eksisterer ikke!");
}
```

I dette eksempelet bruker vi `require`-funksjonen til å importere FileSystem-modulen. Deretter definerer vi stien for mappen vi vil sjekke og bruker `fs.existsSync()` til å sjekke om den eksisterer. Til slutt gir vi en beskjed til brukeren basert på resultatet av sjekken.

## Dypdykk
Det er verdt å merke seg at `fs.existsSync()`-funksjonen sjekker både for eksistensen av mappen og tilgangstilatelsene for å lese denne mappen. Det betyr at selv om mappen eksisterer kan du få en `false`-verdi hvis brukeren som kjører koden din ikke har tilgang til å lese mappen.

I tillegg kan du også bruke `fs.statSync()` for å få mer informasjon om mappen, som for eksempel størrelse, opprettelsesdato og sist endret dato. Denne funksjonen gir deg en `Stats`-objekt som kan utforskes for å få tilgang til disse detaljene.

```Javascript
// Henter stats for mappen
const stats = fs.statSync(mappesti);

// Viser mappens størrelse
console.log("Mappen er " + stats.size + " bytes stor.");
```

## Se også
- [Node.js FileSystem-modulen](https://nodejs.org/api/fs.html)
- [Dokumentasjon for fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Dokumentasjon for fs.statSync()](https://nodejs.org/api/fs.html#fs_fs_statsync_path_options)