---
title:                "TypeScript: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang kommet over et stort datasett i et eksportert CSV-format og blitt overveldet av tanken på å sortere og analysere det manuelt? Vel, hvis du er en utvikler som er kjent med TypeScript, er det ingen grunn til bekymring! CSV-filer kan være det perfekte utgangspunktet for å lage automatiserte data analyser og rapporter ved hjelp av TypeScript. Les videre for å lære hvordan du kan bruke dine programmeringsferdigheter til å arbeide med CSV-filer.

## Hvordan

For å begynne å jobbe med CSV-filer i TypeScript, må du inkludere "fs" og "csv-parser" modulene i prosjektet ditt. Dette kan gjøres ved å installere dem via npm og deretter bruke require-funksjonen til å importere dem inn i koden din.

```TypeScript
const fs = require('fs');
const csv = require('csv-parser');
```

Nå kan du bruke fs modulen til å lese CSV-filen og csv-parser-modulen til å analysere den. La oss for eksempel si at vi har en CSV-fil som inneholder informasjon om ansatte i en bedrift, med kolonnene "navn", "stilling" og "lønn". Vi kan bruke følgende kode til å lese filen og logge ut informasjonen for hver ansatt til konsollen:

```TypeScript
fs.createReadStream('ansatte.csv')
  .pipe(csv())
  .on('data', (data) => {
    console.log(`${data.navn} har stillingen ${data.stilling} og tjener ${data.lønn} kroner i måneden.`)
  })
```

Output vil se slik ut:

```
John Doe har stillingen IT-konsulent og tjener 50000 kroner i måneden.
Jane Smith har stillingen Markedsføringsleder og tjener 60000 kroner i måneden.
Bob Johnson har stillingen Salgssjef og tjener 70000 kroner i måneden.
```

Som du kan se, kan vi enkelt få tilgang til dataene fra CSV-filen og bruke dem til å lage nyttig informasjon.

## Deep Dive

Hvis du ønsker å gå dypere inn i arbeidet med CSV-filer i TypeScript, er det flere nyttige metoder og funksjoner du kan utforske. For eksempel kan du bruke "csv-writer" modulen til å skrive data fra et JSON-objekt til en CSV-fil, eller du kan bruke "fast-csv" modulen for å håndtere store CSV-filer mer effektivt.

Det er også verdt å sette seg inn i forskjellige formaterings- og parsingalternativer for CSV-filer, for å håndtere eventuelle spesifikke behov for prosjektet ditt. Dette kan inkludere å endre salgsseparatorer eller bestemme header informasjon for filen.

Uansett hvilket nivå du ønsker å jobbe med CSV-filer i TypeScript, er det viktig å huske på å håndtere eventuelle feil og unntak som kan oppstå når du arbeider med dataene. Dette vil sikre at du har pålitelige og nøyaktige resultater.

## Se også

- [npm: fs](https://www.npmjs.com/package/fs)
- [npm: csv-parser](https://www.npmjs.com/package/csv-parser)
- [npm: csv-writer](https://www.npmjs.com/package/csv-writer)
- [npm: fast-csv](https://www.npmjs.com/package/fast-csv)