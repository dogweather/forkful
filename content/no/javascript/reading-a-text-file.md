---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

---

## Hva og Hvorfor?
Å lese en tekstfil er prosessen med å hente og tolke data lagret i et vanlig tekstformat i en fil. Programmere gjør dette for å manipulere data, analysere informasjon, og mange andre oppgaver som medfører behandling av lagret tekst.

## Hvordan:
Her er en enkel kode i Javascript for å lese innholdet i en tekstfil ved hjelp av Node.js 'fs' modul:

```Javascript
const fs = require('fs');

fs.readFile('MinTekstFil.txt', 'utf8', function(err, data){
    if (err) throw err;
    console.log(data);
});
```

Når du kjører denne koden, vil den skrive ut innholdet i teksten 'MinTekstFil.txt' til konsollen.

## Dypdykk:
Historisk sett, lesingen av tekstfiler går helt tilbake til de tidlige dagene av programmering. Lesing av tekstfiler er en grunnleggende operasjon som har blitt optimert og forenklet over tid.

Alternativt, i nettleserbasert Javascript, kan du bruke File API for å lese tekstfiler. Her er et eksempel på det:

```Javascript
let fileInput = document.getElementById('myFile');
let file = fileInput.files[0];
let reader = new FileReader();

reader.onload = function(e){
  console.log(reader.result);
}

reader.readAsText(file);
```
Vær oppmerksom på at 'myFile' skal være ID-en til en `<input type="file">` i HTML-en din.

Implementering av lesing av tekstfiler i Javascript avhenger sterkt av miljøet; Node.js vil bruke 'fs' modul, mens nettlesere vil bruke File API. Begge er effektive, men de har litt forskjellige brukssituasjoner.

## Se Også:
- MDN Web Docs: [FileReader.readAsText()](https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readAsText)
- Node.js Docs: [fs.readFile()](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- W3Schools: [JavaScript HTML DOM Input Text](https://www.w3schools.com/jsref/dom_obj_text.asp)