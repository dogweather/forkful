---
title:                "Javascript: Sjekke om en mappe eksisterer"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig å sjekke om en mappe eksisterer i Javascript for å sikre at koden kjører uten feil og hindre potensielle feil i fremtiden.

## Hvordan

For å sjekke om en mappe eksisterer i Javascript, kan du bruke fs modulet, som gir tilgang til operativsystemets filsystem. Vi kan bruke fs.stat() metoden for å sjekke om en mappe eksisterer. La oss se på et eksempel:

```Javascript
const fs = require('fs');

fs.stat('./mappe', (err, stats) => {
  if (err) {
    console.log('Mappen eksisterer ikke');
  } else {
    console.log('Mappen eksisterer');
  }
});
```

Output vil være enten "Mappen eksisterer ikke" eller "Mappen eksisterer" avhengig av om mappen faktisk eksisterer eller ikke. I koden over bruker vi fs.stat() metoden og gir den navnet på mappen vi ønsker å sjekke. Hvis det oppstår en feil, vil vi få en "err" som argument i tilbakekallfunksjonen, og hvis mappen eksisterer vil vi få informasjon om den i "stats" argumentet. 

## Dypdykk

Det finnes flere metoder for å sjekke om en mappe eksisterer i Javascript. Vi kan også bruke path modulet for å jobbe med filstier, og da spesifikt fs.existsSync() metoden for å sjekke om en sti eksisterer. En annen metode er å bruke try-catch blokker rundt koden som prøver å utføre operasjoner på mappen, og fange eventuelle feil som oppstår.

Det er også viktig å merke seg at disse metodene kan variere i funksjonalitet avhengig av operativsystemet og versjonen av Node.js som kjøres.

## Se også

- [fs module documentation](https://nodejs.org/api/fs.html)
- [path module documentation](https://nodejs.org/api/path.html)
- [fs.stat() method documentation](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)
- [fs.existsSync() method documentation](https://nodejs.org/api/fs.html#fs_fs_existssync_path)