---
title:                "Javascript: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med data, spesielt store mengder data, er det sannsynligvis at du har hørt om CSV-filer. CSV står for "Comma Separated Values" og er en vanlig måte å lagre og utveksle data på. Men hvorfor ville du som programmerer, ønske å involvere deg med CSV?

En av hovedgrunnene er at CSV er en enkel og effektiv måte å strukturere og organisere data på. Det er en tekstbasert fil som lagrer data i form av rader og kolonner, og kan enkelt leses og manipuleres av dataprogrammer. Dette gjør det til et ideelt format for å importere og eksportere data til og fra forskjellige programmer og systemer.

## Slik gjør du det

Hvis du vil jobbe med CSV-filer i Javascript, er det flere måter å gjøre det på. En enkel måte er å bruke et bibliotek som heter "csv-parse", som tillater deg å lese og manipulere CSV-filer ved hjelp av Javascript-kode.

La oss si at vi har en CSV-fil kalt "personer.csv" som inneholder informasjon om personer som:

| Navn  | Alder | Kjønn |
| ------| ----- | ------|
| Mari  | 25    | Kvinne|
| Per   | 30    | Mann  |
| Kari  | 20    | Kvinne|

Vi kan bruke csv-parse for å lese denne filen og utføre forskjellige operasjoner på dataene. For eksempel kan vi skrive ut alle kvinnelige personer fra filen ved å bruke følgende kode:

```Javascript
const parser = require('csv-parse');
const fs = require('fs');

fs.createReadStream('personer.csv')
  .pipe(parser({ delimiter: ',' }))
  .on('data', (row) => {
    // Sjekker om personen er en kvinne
    if (row[2] === 'Kvinne') {
      console.log(`${row[0]} er en kvinne på ${row[1]} år`);
    }
  });
```

Denne koden vil skrive ut følgende:

```
Mari er en kvinne på 25 år
Kari er en kvinne på 20 år
```

Vi kan også bruke csv-parse for å konvertere CSV-dataene til JSON-format, noe som kan være nyttig hvis vi trenger å arbeide med dataene i et annet programmeringsspråk eller format.

## Dypdykk

Det er viktig å merke seg at CSV-filer kan ha forskjellige formater og spesifikasjoner, og det er viktig å forstå disse for å kunne jobbe effektivt med dem. For eksempel kan noen CSV-filer bruke forskjellige separatorer som semikolon eller tabulator istedenfor komma. Det er også viktig å være oppmerksom på eventuelle spesielle tegn eller escape-karakterer som kan være tilstede i filen.

En annen ting å huske på er at CSV-filer kan inneholde store datamengder, og det kan være en utfordring å håndtere disse effektivt. Det kan være lurt å begrense antall rader som blir lest og behandlet på en gang, spesielt hvis du jobber med en svært stor fil.

## Se også

- [csv-parse dokumentasjon](https://csv.js.org/parse/)
- [Viktigheten av å forstå forskjellige CSV-formater](https://www.educba.com/csv-file-formats/)
- [Behandling av store datamengder effektivt](https://www.toptal.com/big-data/handling-large-datasets-efficiently)