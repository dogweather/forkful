---
title:                "Javascript: Oppretting av midlertidig fil"
simple_title:         "Oppretting av midlertidig fil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor?

Å lage midlertidige filer i JavaScript kan være nyttig når du vil lagre data midlertidig mens du kjører en prosess eller lagre data som du bare trenger for en kort periode. Dette kan også være nyttig når du jobber med store datamengder som du ikke ønsker å lagre permanent på enheten din.

## Slik gjør du det:

```Javascript
// Opprett en midlertidig fil ved hjelp av "fs" -modulen i Node.js
var fs = require('fs');

// Opprett en enkel tekstfil
fs.writeFileSync('midlertidigFil.txt', 'Dette er en midlertidig fil');

// Les innholdet i filen og skriv det ut i konsollen
var innhold = fs.readFileSync('midlertidigFil.txt', 'utf8');
console.log(innhold);

// Slett den midlertidige filen
fs.unlinkSync('midlertidigFil.txt');
```

Output:
```
Dette er en midlertidig fil
```

## Dypt dykk:

Å lage midlertidige filer i JavaScript er vanligvis enkelt, men det er noen ting du bør være oppmerksom på. For det første, når du oppretter en midlertidig fil, må du være sikker på at navnet ikke kolliderer med noen eksisterende filer. Det kan være lurt å generere et tilfeldig navn eller legge til en unik identifikator for å unngå eventuelle problemer. I tillegg må du sørge for å slette den midlertidige filen når du er ferdig med å bruke den for å unngå å fylle opp enheten din med unødvendige filer.

Når det gjelder å lagre store datamengder midlertidig, kan du også vurdere å bruke en databaseløsning som MongoDB eller Firebase, i stedet for å lage en midlertidig fil. Dette kan være mer hensiktsmessig for å håndtere store datamengder og gir deg mer fleksibilitet når det gjelder å hente og behandle dataene.

## Se også:

- [Node.js "fs" modul](https://nodejs.org/api/fs.html)
- [Hvordan generere tilfeldige tal i JavaScript](https://www.w3schools.com/js/js_random.asp)
- [Introduksjon til MongoDB](https://www.mongodb.com/what-is-mongodb)
- [Hva er Firebase?](https://firebase.google.com/docs)