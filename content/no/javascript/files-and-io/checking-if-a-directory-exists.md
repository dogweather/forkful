---
title:                "Sjekker om en mappe eksisterer"
aliases:
- /no/javascript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:45.985179-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sjekker om en mappe eksisterer"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å sjekke om en mappe finnes i JavaScript er essensielt for oppgaver som involverer manipulasjon av filer, slik at skript kan verifisere mappens tilstedeværelse før man leser fra eller skriver til den. Denne operasjonen forhindrer feil og sikrer en jevnere kjøring av programmet, spesielt i applikasjoner som dynamisk håndterer filer eller mapper basert på brukerinndata eller eksterne datakilder.

## Hvordan:
I Node.js, siden JavaScript i seg selv ikke har direkte tilgang til filsystemet, brukes `fs`-modulen typisk for slike operasjoner. Her er en enkel måte å sjekke om en mappe finnes ved bruk av `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Sjekk om mappen finnes
if (fs.existsSync(directoryPath)) {
  console.log('Mappen finnes.');
} else {
  console.log('Mappen finnes ikke.');
}
```
**Eksempel på utskrift:**
```
Mappen finnes.
```
Eller, for en ikke-blokkerende asynkron tilnærming, bruk `fs.promises` med `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Mappen finnes.');
  } catch (error) {
    console.log('Mappen finnes ikke.');
  }
}

checkDirectory('./sample-directory');
```
**Eksempel på utskrift:**
```
Mappen finnes.
```

For prosjekter som i stor grad benytter seg av fil- og mappeoperasjoner, tilbyr `fs-extra`-pakken, en utvidelse av den native `fs`-modulen, praktiske tilleggsfunksjoner. Her er hvordan du kan oppnå det samme med `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Sjekk om mappen finnes
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Mappen finnes.' : 'Mappen finnes ikke.'))
  .catch(err => console.error(err));
```
**Eksempel på utskrift:**
```
Mappen finnes.
```

Denne tilnærmingen muliggjør ren, lesbar kode som smidig integreres med moderne JavaScript-praksiser.
