---
title:                "Arbeid med csv"
html_title:           "Javascript: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen ønske å jobbe med CSV-formatet? Vel, dette formatet er enkelt å forstå og brukes ofte til å lagre og dele data. Det er også utbredt innenfor webutvikling og kan være svært nyttig for å håndtere store datasett.

## Slik gjør du det
Å arbeide med CSV i Javascript er enkelt og krever bare noen få linjer med kode. Først må vi laste inn et bibliotek, for eksempel "csv-parser", ved hjelp av npm. Deretter kan vi begynne å bruke funksjonene til dette biblioteket:

```Javascript
const csv = require('csv-parser');
const fs = require('fs');

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (row) => {
    // Gjør noe med hver rad i CSV-filen
    console.log(row);
  })
  .on('end', () => {
    // Ferdig!
    console.log('Lesing av CSV-fil fullført.');
  });
```

Med denne koden leser vi innholdet i en CSV-fil og utfører en handling på hver rad. Vi kan også bruke andre funksjoner fra "csv-parser" for å filtrere, sortere eller endre dataene i filen.

## Dypdykk
Nå som vi har fått en enkel forståelse for å arbeide med CSV-filer i Javascript, kan vi gå dypere inn i dette formatet. CSV står for "Comma Separated Values" og er en måte å organisere og lagre data på. Hver rad i en CSV-fil representerer en rekke data-verdier, og hver verdi er adskilt med et komma.

Selv om CSV kan virke ganske enkelt, er det noen fallgruver å se opp for. For eksempel kan det være vanskelig å håndtere data som inneholder komma eller linjeskift, og noen ganger må vi bruke spesifikke kodingssystemer for å sikre at dataene våre er riktig representert.

## Se også
- [csv-parser pakken på npm](https://www.npmjs.com/package/csv-parser)
- [Offisiell CSV-nettside](https://en.wikipedia.org/wiki/Comma-separated_values)
- [En enkel guide for å jobbe med CSV-filer i Javascript](https://www.syncfusion.com/blogs/post/working-with-csv-files-using-javascript.aspx)