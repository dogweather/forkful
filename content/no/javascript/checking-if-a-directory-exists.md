---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:57:14.227289-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekking av om en katalog eksisterer innebærer å finne ut om en bestemt mappe finnes på filsystemet. Programmere gjør dette for å unngå feil, som å prøve å lese fra eller skrive til en ikke-eksisterende katalog.

## Slik gjør du:
For å sjekke om en katalog eksisterer i nyere versjoner av Node.js, bruk `fs.existsSync` eller asynkron `fs.access` sammen med `fs.constants.F_OK`.

```javascript
const fs = require('fs');

// Synkron metode
const directoryExistsSync = fs.existsSync('/path/to/directory');
console.log(directoryExistsSync); // Output: true eller false

// Asynkron metode
fs.access('/path/to/directory', fs.constants.F_OK, (err) => {
    const directoryExistsAsync = !err;
    console.log(directoryExistsAsync); // Output: true eller false
});
```

## Dypdykk
Tidligere, brukte programmere `fs.exists`, men dette er nå foreldet på grunn av sin ikke-standard tilbakekallingsargumenter og uforutsigbar natur. Selv `fs.existsSync` kan potensielt føre til ytelsesproblemer da det er synkront og blokkerer event-loopen. Asynkrone alternativer som `fs.access` med `fs.constants.F_OK` er foretrukket for non-blocking kode.

Noen ganger bruker programmerere tredjeparts bibliotek som `fs-extra` eller frameworks som gir abstraksjoner over filsystemet, for mer komfortabel håndtering av slike operasjoner.

Det er også verdt å merke seg at på grunn av race conditions kan en katalog som sjekkes for eksistens bli slettet før den faktisk brukes, så noen situasjoner kan kreve ytterligere håndtering av slike tilfeller.

## Se også
- Node.js `fs` dokumentasjon: https://nodejs.org/api/fs.html
- `fs-extra` modulen: https://www.npmjs.com/package/fs-extra
- Artikkel om å håndtere race conditions: https://nodejs.org/en/docs/guides/race-conditions/
