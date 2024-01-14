---
title:                "TypeScript: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er et viktig aspekt av programmering. Det kan hjelpe deg med å sørge for at filer eller ressurser er på riktig sted før du prøver å åpne eller hente dem. Det er også nødvendig for å sørge for at programmet ditt fungerer som det skal og unngå feilmeldinger.

## Hvordan gjøre det

For å sjekke om en mappe eksisterer, kan vi bruke 'fs' biblioteket i TypeScript. Først må vi importere dette biblioteket:

```TypeScript
import * as fs from 'fs';
```

Deretter kan vi bruke 'fs.existsSync()' funksjonen for å sjekke om en mappe eksisterer på en gitt bane. Her er et eksempel som sjekker om mappen 'documents' eksisterer i brukerens hjemmemappe og returnerer true eller false basert på resultatet:

```TypeScript
if (fs.existsSync('/home/brukernavn/documents')) {
    console.log('Mappen eksisterer');
} else {
    console.log('Mappen eksisterer ikke');
}
```

## Dypdykk

Det er noen få ting du bør være oppmerksom på når du sjekker om en mappe eksisterer. For det første, hvis mappen du sjekker ligger i en annen mappe enn brukerens hjemmemappe, må du huske å inkludere hele banen til mappen. Dette kan være spesielt viktig hvis du distribuerer programmet ditt til forskjellige miljøer.

I tillegg er det viktig å huske at 'fs.existsSync()' bare sjekker om en mappe eksisterer, ikke om den er skrivbar eller om du har tillatelse til å åpne den. Det kan være lurt å inkludere ytterligere validering og håndtering for disse scenariene.

## Se også

* [fs.existsSync() i Node.js dokumentasjon](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
* [Sjekk om fil eller mappe eksisterer i TypeScript ](https://www.digitalocean.com/community/tutorials/how-to-use-the-file-system-in-node-js)
* [Bruke filsystemet modulen i TypeScript](https://www.digitalocean.com/community/tutorials/reading-and-writing-files-with-node-js)