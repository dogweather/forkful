---
title:    "Javascript: Opprette en midlertidig fil"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Hvorfor

Temporære filer kan være nyttige når du jobber med programmering. De tillater deg å midlertidig lagre data eller informasjon som trengs for at programmet ditt skal kunne kjøre. Dette kan være spesielt nyttig når du jobber med større datamengder eller når du ikke ønsker å skrive data permanent til en fil.

# Hvordan

For å opprette en temporær fil i Javascript kan du bruke følgende kode:

```Javascript
const fs = require('fs'); // Importerer fs-modulen for å kunne jobbe med filer
const path = require('path'); // Importerer path-modulen for å kunne håndtere filsti
const tmpFile = path.join(__dirname, 'tempfile.txt'); // Setter filsti til å være i samme mappe som Javascript-filen

fs.writeFile(tmpFile, 'Dette er en temporær fil', (err) => { // Skriver data til filen
  if (err) throw err;
  console.log('Temporær fil opprettet');
});
```

Koden over vil opprette en ny fil kalt "tempfile.txt" i samme mappe som Javascript-filen. Den vil deretter skrive teksten "Dette er en temporær fil" til filen. Hvis det ikke oppstår noen feil, vil du se en melding i konsollen som indikerer at filen er opprettet.

# Dypdykk

Når du jobber med temporære filer, er det viktig å huske på at de ikke trenger å være permanente. Dette betyr at du bør slette dem når de ikke lenger er nødvendige. I Javascript kan du gjøre dette ved å bruke følgende kode:

```Javascript
fs.unlink(tmpFile, (err) => { // Sletter filen
  if (err) throw err;
  console.log('Temporær fil slettet');
});
```

Det er også viktig å merke seg at navnet på den temporære filen ikke trenger å være "tempfile.txt". Du kan gi den et hvilket som helst navn, så lenge det er unikt og du husker navnet når du skal slette den.

# Se også

- Documentasjon for fs-modulen i Node.js: https://nodejs.org/api/fs.html
- Tutorial om å opprette og slette filer i Node.js: https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-node-js