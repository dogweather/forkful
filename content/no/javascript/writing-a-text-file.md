---
title:    "Javascript: Å skrive en tekstfil"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange av oss som driver med programmering vil før eller senere måtte skrive en tekstfil. Dette kan være for å lagre data, konfigurasjonsinformasjon, eller til og med kode. Å kunne skrive en tekstfil kan være en viktig ferdighet i programmering, og det kan også være nyttig i hverdagen for å lagre informasjon eller lage notater. I denne bloggposten vil vi gå gjennom hvordan du kan skrive en tekstfil ved hjelp av Javascript.

## Hvordan gjøre det
Først må vi starte med å opprette en fil og skrive innholdet i filen. Dette kan gjøres ved å bruke et av Javascripts innebygde funksjoner, ```writeFile()```. Denne funksjonen tar inn to parametere: filnavnet og innholdet som skal skrives til filen.

Et eksempel på hvordan dette kan gjøres:

```Javascript
const fs = require('fs'); // importere file system (fs) modulen 

// Opprette en fil og legge til innhold 
fs.writeFile('hello.txt', 'Hei verden!', (err) => {
  if (err) throw err; // kaste feilmelding dersom det oppstår en feil 
  console.log('Filen hello.txt ble opprettet.'); // skrive ut en bekreftelse 
}); 
```

Kjører vi denne koden vil det opprette en fil med navnet "hello.txt" og skrive teksten "Hei verden!" inn i filen. Dersom filen allerede eksisterer, vil innholdet i filen bli erstattet med det nye innholdet vi har gitt.

## Dykk dypere
Nå når vi vet hvordan vi kan skrive en tekstfil med Javascript, kan vi utforske mer avanserte funksjoner og muligheter. Ved hjelp av ulike moduler og metoder kan vi for eksempel endre formatet på innholdet i filen, lese fra en eksisterende fil, eller legge til mer avansert formatering som HTML.

Det er også viktig å lære å håndtere feil når man skriver en tekstfil. Dersom filen ikke kan åpnes eller skrives til, må vi ha en måte å håndtere dette på og gi en brukervennlig feilmelding.

## Se også
- [fs module documentation (fs modulen dokumentasjon)](https://nodejs.org/api/fs.html)
- [Node.js beginner tutorial (Node.js nybegynnerguide)](https://nodejs.dev/learn)
- [How to write to a file using pure Javascript (Slik skriver du til en fil med ren Javascript)](https://attacomsian.com/blog/javascript-write-to-file)