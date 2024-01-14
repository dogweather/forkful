---
title:    "Javascript: Lesing av en tekstfil"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil kan være en nyttig ferdighet for alle som er interessert i å lære å kode. Det å kunne lese og håndtere ulike typer filer er en viktig del av programmering, og tekstfiler er en av de vanligste filtypene som brukes.

## Slik gjør du det

For å lese en tekstfil i Javascript, kan du bruke inbygde funksjoner som fs.readFileSync() eller fs.readFile(). Du må først importere "fs" modulen i koden din for å bruke disse funksjonene.

```Javascript
const fs = require('fs');

// Leser en tekstfil synchronously
let data = fs.readFileSync('tekstfil.txt', 'utf8');
console.log(data); // Skriver ut innholdet i konsollen

// Leser en tekstfil asynchronously
fs.readFile('tekstfil.txt', 'utf8', (err, data) => {
  if (err) console.error(err); // Håndterer eventuelle feil
  console.log(data); // Skriver ut innholdet i konsollen
})
```

I dette eksempelet bruker vi en tekstfil kalt "tekstfil.txt", men du kan selvfølgelig bruke navnet på din egen fil i koden. Vær også oppmerksom på at readFile() funksjonen tar imot en callback-funksjon som vil bli eksekvert når fillesingen er ferdig.

## Dypdykk

Nå som vi har sett på hvordan man leser en tekstfil, la oss ta en dypere titt på hva som faktisk skjer bak kulissene. Først og fremst, en tekstfil består av en rekke byter, og hver byte har en tilsvarende karakterkoding. Den mest vanlige karakterkodingen for tekstfiler er UTF-8. Når vi leser en tekstfil i Javascript, vil filen bli konvertert til UTF-8 formatet for å kunne leses riktig.

En annen ting å være klar over er at når vi bruker readFile() funksjonen, vil filen bli lest asynkront, noe som betyr at resten av koden vil fortsette å kjøre mens filen blir lest i bakgrunnen. Derfor bruker vi en callback-funksjon for å sikre at vi får tilgang til filens innhold når det er ferdig å lese.

## Se også

* [fs modulen i Node.js docs] (https://nodejs.org/api/fs.html)
* [Eksempelkode for å lese en tekstfil] (https://www.digitalocean.com/community/tutorials/how-to-read-a-file-with-node-js)
* [Intro til filbehandling i Javascript] (https://www.w3schools.com/js/js_file_handling.asp)