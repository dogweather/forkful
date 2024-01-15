---
title:                "Å jobbe med csv"
html_title:           "TypeScript: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med CSV-filer er en vanlig oppgave i programmering, spesielt når du håndterer store mengder data. Ved å lære å arbeide med CSV-filer i TypeScript, kan du enklere importere og eksportere data til og fra forskjellige systemer, og effektivisere arbeidet ditt.

## Hvordan gjøre det

For å begynne å jobbe med CSV-filer i TypeScript, trenger du en pakke som heter "csv-parser". Denne pakken lar deg enkelt lese og behandle data fra CSV-filer.

Først må du installere pakken ved å kjøre følgende kommando i terminalen din:

```
npm install csv-parser
```

Deretter kan du importere den i TypeScript-filen din ved å inkludere følgende linje på toppen av filen:

```TypeScript
import * as csv from 'csv-parser';
```

For å lese en CSV-fil, kan du bruke følgende kode:

```TypeScript
fs.createReadStream('data.csv') // åpner filen for lesing
  .pipe(csv()) // bruker csv-parser for å parse data
  .on('data', (row) => { // tar i mot hver linje som et objekt
    console.log(row); // skriver ut objektet i konsollen
  })
  .on('end', () => { // kalt når hele filen er lest
    console.log('Done');
  });
```

Dette vil skrive ut hver linje i CSV-filen som et JavaScript-objekt i konsollen din. Du kan deretter behandle dataene videre som du ønsker.

For å eksportere data til en CSV-fil, kan du bruke følgende kode:

```TypeScript
import * as fs from 'fs';

const data = ["Navn, Alder", // oppretter en array med dataene du vil eksportere
              "Per, 30",
              "Kari, 25",
              "Ola, 28"];

fs.writeFile('eksportert_data.csv', data.join('\n'), (err) => { // skriver dataene til en ny fil
  if (err) throw err;
  console.log('Data eksportert!');
});
```

Dette vil opprette en ny CSV-fil med navnet "eksportert_data.csv" og skrive dataene du har angitt til den.

## Dykk dypere

Når du arbeider med CSV-filer, er det viktig å være klar over at ikke alle verdier er like. Noen ganger kan verdier være tomme, eller de kan inneholde komma eller anførselstegn. I slike tilfeller må du ta hensyn til dette når du leser og behandler dataene.

En annen ting å være klar over er at batch-lesing og skriving av store CSV-filer kan føre til minnelekkasje. Det kan være lurt å vurdere å bruke streaming-tilnærmingen som er vist i eksemplene ovenfor, for å unngå dette problemet.

## Se også

- [csv-parser npm-pakke](https://www.npmjs.com/package/csv-parser)
- [Offisiell TypeScript-dokumentasjon](https://www.typescriptlang.org/docs/)