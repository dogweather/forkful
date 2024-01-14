---
title:                "TypeScript: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange grunner til å laste ned en nettside, enten det er for personlig bruk eller profesjonelt. Noen ganger kan nettverket være tregt og det er mer effektivt å lagre nettsiden lokalt for å raskere få tilgang til den senere. I tillegg kan det være nyttig å kunne lage en offline versjon av en nettside for å kunne vise den til andre uten å være koblet til internett. Uansett hva grunnen er, kan det være nyttig å kunne laste ned en nettside ved hjelp av TypeScript.

## Hvordan gjøre det
For å laste ned en nettside ved hjelp av TypeScript, trenger du først å importere 'http' og 'fs' modulene. Disse vil tillate oss å lage HTTP-forespørsler og lagre filer på datamaskinen vår. Deretter kan vi bruke 'http.get()' funksjonen for å lage en GET-forespørsel til nettsiden vi ønsker å laste ned. Dette vil returnere et responsobjekt som vi kan behandle ved hjelp av 'on' metoden og 'pipe' metoden for å skrive dataen til en fil. Under følger et eksempel på hvordan dette kan gjøres:

```TypeScript
import * as http from 'http';
import * as fs from 'fs';

const url = 'https://www.example.com';
const filePath = 'example.html';

http.get(url, (res) => {
  // Create a file stream
  const file = fs.createWriteStream(filePath);
  
  // Write response data to file
  res.on('data', (data) => {
    file.write(data);
  });
  
  // Save file when response ends
  res.on('end', () => {
    file.end();
    console.log(`Nettside lastet ned til ${filePath}`);
  });
}).on('error', (err) => {
  console.error(err.message);
});
```

Dette vil opprette en fil kalt 'example.html' og lagre nettsiden fra 'url' dit. Du kan endre 'url' og 'filePath' variablene for å laste ned andre nettsider og lagre dem under forskjellige filnavn.

## Dypdykk
HTTP-forespørsler er en fundamental del av å laste ned en nettside. Når vi bruker 'http.get()' funksjonen, sender vi en GET-forespørsel til målnettstedet. Dette vil returnere en respons som inneholder statuskoden, headers og eventuell data fra nettsiden. Ved å bruke 'on' metoden, kan vi lytte etter 'data' og 'end' hendelser for å behandle responsen og lagre dataen. Vi kan også bruke 'pipe' metoden til å skrive dataen direkte til en fil, noe som forenkler koden vår.

Det er også verdt å merke seg at denne metoden ikke vil laste ned den komplette nettsiden med mindre den er statisk. Dynamiske nettsider som bruker JavaScript vil bare laste ned det grunnleggende HTML-innholdet, og javascriptet vil ikke bli utført. Hvis du ønsker å laste ned en komplett dynamisk nettside, kan du bruke et verktøy som Puppeteer som styrer en nettleser og lar deg laste ned hele nettsiden sammen med all dens dynamiske innhold.

## Se også
- [Node.js dokumentasjon for 'http' modulen](https://nodejs.org/api/http.html)
- [Node.js dokumentasjon for 'fs' modulen](https://nodejs.org/api/fs.html)
- [TypeScript dokumentasjon for strømmer](https://www.typescriptlang.org/docs/handbook/2/streams.html)
- [Puppeteer dokumentasjon](https://pptr.dev/)