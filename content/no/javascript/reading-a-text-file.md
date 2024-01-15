---
title:                "Lesing av en tekstfil"
html_title:           "Javascript: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen bry seg om å lese en tekstfil? Vel, det er ganske enkelt. Å kunne lese en tekstfil er en viktig ferdighet for de som jobber med webutvikling eller programmering. Tekstfiler er ofte brukt til å lagre data, konfigurasjoner og andre viktige opplysninger som trengs for å drive et program eller nettsted.

# Hvordan gjøre det

Det er flere måter å lese en tekstfil på i Javascript, men jeg vil vise deg den enkleste og mest effektive måten. Først må du åpne filen du vil lese ved å bruke `fs`-modulen. Så kan du bruke `readFileSync()`-funksjonen, som vil lese hele filen og returnere innholdet som en streng. 

```javascript
const fs = require('fs');
let content = fs.readFileSync('filnavn.txt', 'utf8');
console.log(content);
```

I eksempelet over åpner vi en fil med navnet "filnavn.txt" og bruker `console.log()` for å skrive ut innholdet på konsollen. Husk å erstatte "filnavn.txt" med navnet på den faktiske filen du vil lese.

# Dykk dypere

Nå som du vet hvordan du kan lese en tekstfil, la oss dykke litt dypere inn i funksjonen `readFileSync()`. Denne funksjonen tar inn to parametere: filnavnet og tegnkodingen som skal brukes. Hvis du ikke spesifiserer tegnkoding, vil standard tegnkodingen være `utf8`. Hvis du for eksempel skulle prøve å lese en fil som bruker en annen tegnkoding, som `latin1`, må du spesifisere det i funksjonen:

```javascript
const fs = require('fs');
let content = fs.readFileSync('filnavn.txt', 'latin1');
console.log(content);
```

Det er også viktig å merke seg at `readFileSync()`-funksjonen vil kaste en feil hvis den ikke kan lese filen av en eller annen grunn. Derfor bør du alltid inkludere en `try...catch`-blokk for å håndtere eventuelle feil som kan oppstå.

# Se også

- [fs-modulen i Node.js dokumentasjon](https://nodejs.org/api/fs.html)
- [Begynnerguide for å lese og skrive filer i Node.js](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-node-js)

Takk for at du leste! Håper denne artikkelen var nyttig for deg i din læringsprosess. Fortsett å øve på å lese filer, og du vil snart være en ekspert på dette området. Lykke til!