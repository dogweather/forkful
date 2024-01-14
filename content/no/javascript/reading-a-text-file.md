---
title:                "Javascript: Lesing av tekstfil"
simple_title:         "Lesing av tekstfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing av tekstfiler er en grunnleggende ferdighet innenfor programmering og kan være nyttig for å lese og behandle datafiler, konfigurasjonsfiler og mer. Det er viktig for enhver programmerer å ha kunnskap om hvordan man leser tekstfiler i Javascript.

## Hvordan

Det finnes flere måter å lese tekstfiler i Javascript, men her vil vi fokusere på en enkel og grunnleggende metode ved hjelp av fs-modulen.

Først må du importere fs-modulen ved å bruke `require`-funksjonen:

```Javascript
const fs = require('fs');
```

Deretter kan du lese en tekstfil ved hjelp av `readFile`-funksjonen, som tar inn filnavnet og en callback-funksjon som argumenter:

```Javascript
fs.readFile('test.txt', (err, data) => {
    if(err) throw err;
    console.log(data); // viser innholdet i tekstfilen
});
```

Hvis alt går som planlagt, vil filinnholdet bli lagret i `data`-variabelen og kan behandles videre etter behov.

## Dykk ned

Det er viktig å være klar over at lesing av tekstfiler i Javascript må håndteres asynkront, da fileravlesningen kan ta litt tid og ikke vil blokkere andre kodelinjer. I eksempelet ovenfor brukte vi en callback-funksjon for å håndtere eventuelle feil og vise resultatet når lesingen er fullført.

Det finnes også andre metoder for å lese tekstfiler, som for eksempel `readFileSync` som vil blokkere kodelinjene til filen er fullstendig lest. Det er viktig å velge den riktige metoden etter behov.

## Se også

- [fs-modulen dokumentasjon (engelsk)](https://nodejs.org/api/fs.html)
- [En komplett guide til å lese og skrive tekstfiler i Javascript (engelsk)](https://www.digitalocean.com/community/tutorials/nodejs-reading-files)