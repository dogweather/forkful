---
title:                "Skrive en tekstfil"
html_title:           "Javascript: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en viktig del av programmering, spesielt i Javascript. Det lar deg lagre og behandle data på en strukturert måte, og er nyttig for å lage og lagre konfigurasjonsfiler og logger.

## Slik gjør du det

For å skrive en tekstfil i JavaScript må du følge noen enkle trinn:

1. Åpne et tekstredigeringsprogram som Notepad, Sublime Text eller Atom.
2. Skriv koden din i .js-filen og lagre den med et passende filnavn og ".js" utvidelse.
3. Skriv følgende linje med kode først for å importere Node's "fs" modulen:

 ```Javascript
 var fs = require('fs');
 ```

4. Deretter kan du bruke fs.writeFile() funksjonen for å skrive innholdet i filen:

 ```Javascript
 fs.writeFile('navn-på-filen.txt', 'Dette er en tekstfil skrevet i JavaScript!', function (err) {
  if (err) return console.log(err);
  console.log('Tekstfilen ble laget!');
});
 ```

5. Åpne filen du nettopp laget og du vil se teksten "Dette er en tekstfil skrevet i JavaScript!" inne i den.

## Dypdykk

Når du skriver en tekstfil i JavaScript, er det viktig å huske på noen nøkkelelementer:

- Du må importere "fs" modulen før du kan skrive til en fil.
- Når du bruker fs.writeFile() funksjonen, må du gi den et filnavn og et innhold å skrive til filen.
- Funksjonen tar også et valgfritt callback-argument, som vil bli kalt når skrivingen er fullført eller hvis det oppstår en feil.

## Se også

- [Node.js fs modul dokumentasjon](https://nodejs.org/api/fs.html)
- [Skape og skrive filer med Node.js](https://www.digitalocean.com/community/tutorials/how-to-create-and-write-to-a-file-in-node-js)