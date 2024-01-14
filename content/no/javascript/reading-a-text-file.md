---
title:    "Javascript: Å lese en tekstfil"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har programmert i Javascript, har du sannsynligvis støtt på situasjoner der du trenger å lese innholdet fra en tekstfil. Dette kan være nyttig for å hente data, konfigurasjonsinnstillinger eller annen informasjon til programmene dine. I denne blogginnlegget vil jeg forklare hvorfor det er viktig å kunne lese tekstfiler, og hvordan du enkelt kan gjøre det med Javascript.

## Hvordan
For å lese en tekstfil med Javascript, trenger du en metode som heter `readFile()` fra `fs` biblioteket. Først må du importere biblioteket ved å bruke `require` kommandoen:

```Javascript
const fs = require('fs');
```

Deretter kan du bruke `readFile()` metoden på følgende måte:

```Javascript
fs.readFile('tekstfil.txt', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Her bruker jeg `tekstfil.txt` som eksempel på navnet til tekstfilen jeg ønsker å lese. Du kan selvfølgelig bytte ut dette med hva som helst annet filnavn du ønsker å lese. Dataen som blir returnert fra `readFile()` er i binær format, så vi må konvertere den til en lesbar streng ved hjelp av `toString()` metoden:

```Javascript
fs.readFile('tekstfil.txt', (err, data) => {
  if (err) throw err;
  console.log(data.toString());
});
```

Dette vil skrive ut innholdet fra tekstfilen i terminalen eller konsollen. Hvis du ønsker å lagre dataen fra filen i en variabel, kan du gjøre det på følgende måte:

```Javascript
let data = fs.readFileSync('tekstfil.txt').toString();
```

Du kan deretter bruke variabelen `data` til å manipulere og bruke innholdet fra tekstfilen i programmet ditt.

## Dypdykk
Nå som vi har sett på hvordan du kan lese en tekstfil med Javascript, kan vi også utforske noen av de andre mulighetene for å arbeide med tekstfiler. I tillegg til å lese filer, kan du også skrive til filer ved hjelp av `writeFile()` metoden fra `fs` biblioteket. Du kan også bruke `appendFile()` metoden for å legge til innhold til slutten av en allerede eksisterende fil. Pass på å lese dokumentasjonen for å lære mer om disse metodene og deres muligheter.

## Se også
- [Node.js FS Modul](https://nodejs.org/api/fs.html)
- [W3Schools Node.js File System](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [MDN Web Docs File System API](https://developer.mozilla.org/en-US/docs/Web/API/File_System_API)