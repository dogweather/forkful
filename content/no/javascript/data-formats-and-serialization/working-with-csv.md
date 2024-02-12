---
title:                "Arbeide med CSV"
aliases:
- /no/javascript/working-with-csv.md
date:                  2024-02-03T19:20:20.964565-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeide med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med CSV (kommaseparerte verdier) i JavaScript innebærer parsing eller generering av CSV-filer for enten å ta inn tabulære data fra eksterne kilder eller eksportere data for bruk i andre programmer. Programmerere gjør dette fordi det muliggjør enkel, lettvekts datautveksling mellom applikasjoner, databaser og systemer der mer komplekse formater som JSON kan være overkill.

## Hvordan:
JavaScript har ikke innebygd CSV-parsing eller strengifisering funksjonalitet slik det har med JSON. Men, du kan enkelt håndtere CSV data ved å bruke enten rå JavaScript for enklere oppgaver eller dra nytte av kraftige biblioteker som `PapaParse` for mer komplekse scenarioer.

### Enkel Parsing med Rå JavaScript
For å parse en enkel CSV-streng til et array av objekter:

```javascript
const csv = `navn,alder,by
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const linjer = csv.split("\n");
  const resultat = [];
  const overskrifter = linjer[0].split(",");

  for (let i = 1; i < linjer.length; i++) {
    const obj = {};
    const gjeldendeLinje = linjer[i].split(",");
    
    for (let j = 0; j < overskrifter.length; j++) {
      obj[overskrifter[j]] = gjeldendeLinje[j];
    }
    resultat.push(obj);
  }
  
  return resultat;
}

console.log(parseCSV(csv));
```
Output:

```
[
  { navn: 'John', alder: '23', by: 'New York' },
  { navn: 'Jane', alder: '28', by: 'Los Angeles' }
]
```

### Enkel Generering til CSV med Rå JavaScript
For å konvertere et array av objekter til en CSV-streng:

```javascript
const data = [
  { navn: 'John', alder: 23, by: 'New York' },
  { navn: 'Jane', alder: 28, by: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(rad => 
    Object.values(rad).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

Output:

```
John,23,New York
Jane,28,Los Angeles
```

### Bruk av PapaParse for Komplekse CSV Oppgaver
For mer komplekse scenarioer er `PapaParse` et robust bibliotek egnet for parsing og strengifisering av CSV-filer med alternativer for strømmer, arbeidere og håndtering av store filer.

Parsing av CSV-fil eller -streng med PapaParse:

```javascript
// Etter å ha lagt til PapaParse i prosjektet ditt
const Papa = require('papaparse');
const csv = `navn,alder,by
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Parsed:", results.data);
  }
});
```

Genererer:

```
Parsed: [
  ["navn", "alder", "by"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

Strengifisering av et array til en CSV-streng med PapaParse:

```javascript
const data = [
  { navn: 'John', alder: 23, by: 'New York' },
  { navn: 'Jane', alder: 28, by: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Generer:

```
navn,alder,by
John,23,New York
Jane,28,Los Angeles
```

Disse eksemplene illustrerer grunnleggende og avansert håndtering av CSV i JavaScript, som muliggjør enkel datautveksling i webapplikasjoner og videre.
