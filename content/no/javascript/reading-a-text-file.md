---
title:                "Leser en tekstfil"
html_title:           "Javascript: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Lesing av en tekstfil er en vanlig oppgave for mange programmerere. Dette innebærer å åpne en fil som inneholder tekst og lese innholdet i programmet. Dette gjøres for å kunne behandle og manipulere teksten, for eksempel å filtrere ut bestemte linjer eller finne spesifikke ord.

## Hvordan:

Lesing av en tekstfil kan gjøres ved hjelp av Javascript ved å bruke innebygde metoder som readFile og readFileSync. Disse metodene tar inn filnavnet som parameter og returnerer en streng med filens innhold. Her er et eksempel:

```Javascript
const fs = require('fs');

// Leser filen asynkront
fs.readFile('tekstfil.txt', (err, data) => {
  if (err) throw err;
  
  // Konverterer dataen til en streng
  let tekst = data.toString();

  // Printer tekststrengen
  console.log(tekst);
});

// Leser filen synkront
let tekst = fs.readFileSync('tekstfil.txt', 'utf8');

// Printer tekststrengen
console.log(tekst);
```

Eksempel på output for tekstfilen "tekstfil.txt":
```
Dette er en tekstfil.
Her er litt tekst som kan leses inn i et program.
Vi håper dette var til hjelp!
```

## Dykk Dypere:

Å lese en tekstfil har vært en vanlig oppgave lenge før Javascript ble opprettet. Med introduksjonen av node.js, ble det enklere å lese filer asynkront og synkront. For å lese filer fra nettleseren, må det brukes spesielle APIer som FileReader eller AJAX.

Det er også mulig å benytte seg av andre programmeringsspråk som Python, som er spesialisert for å arbeide med tekstdokumenter og har mange innebygde funksjoner for dette formålet.

Det er også viktig å huske på at lesing og skriving av filer kan være en ressurskrevende prosess, så det er alltid lurt å lukke filen etter bruk for å unngå tap av ressurser.

## Se også:

- [Node.js dokumentasjon for fs-modul](https://nodejs.org/api/fs.html)
- [W3Schools tutorial om å lese filer i Javascript](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [MDN web docs om FileReader API](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)