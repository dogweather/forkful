---
title:                "Å jobbe med csv"
html_title:           "Javascript: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

CSV står for "Comma Separated Values", og er en vanlig måte å lagre og organisere data på. CSV-filer består av rader og kolonner, der hver kolonne tilsvarer en attributt og hver rad tilsvarer en entry. Programmere jobber med CSV-filer fordi de ofte blir brukt til å importere eller eksportere data fra et program eller å analysere store mengder data.

## Hvordan:

For å kunne jobbe med CSV-filer i Javascript, trenger du et bibliotek som heter "csv-parser". Dette kan enkelt installeres ved å kjøre "npm install csv-parser" i terminalen din. Etter installasjonen kan du bruke dette biblioteket i koden din ved å skrive følgende:

```Javascript
const csv = require('csv-parser');

fs.createReadStream('myCsvFile.csv')
  .pipe(csv())
  .on('data', (row) => {
    // gjør noe med hver rad i CSV-filen
  })
  .on('end', () => {
    // ferdig med å lese gjennom alle radene i filen
  })
```

Dette eksempelet viser hvordan man kan lese data fra en CSV-fil og gjøre noe med hver rad ved å bruke "on('data')" metoden. Det er også mulig å skrive data til en CSV-fil ved å bruke "csv-writer" biblioteket. Du kan utforske flere muligheter ved å lese dokumentasjonen til disse bibliotekene.

## Dypdykk:

CSV-formatet ble først introdusert på 1970-tallet og har blitt en standard for å lagre og utveksle data mellom ulike systemer. Det finnes også flere alternative formater som JSON og XML, men CSV er fortsatt en populær og enkel måte å håndtere data på.

Å jobbe med CSV-filer i Javascript kan være nyttig når man trenger å importere eller eksportere data fra en database eller applikasjon. Det kan også være nyttig når man ønsker å analysere store mengder data, siden CSV-filer er lett å lese og manipulere.

## Se også:

- CSV-parser dokumentasjon: https://csv.js.org/parse/
- CSV-writer dokumentasjon: https://csv.js.org/stringify/