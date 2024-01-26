---
title:                "Arbeid med CSV"
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV står for "Comma Separated Values" og er en drøss med data skilt med komma. Det er populært fordi det er lett å lese og skrive, og det funker med mange ulike programmer, inkludert regneark og databaser.

## Hvordan:
```Javascript
const csv = `Navn,Alder,By
Ola,30,Oslo
Kari,25,Bergen`;

function parseCSV(csvData) {
  const lines = csvData.split("\n");
  const headers = lines[0].split(",");
  const rows = lines.slice(1);
  
  return rows.map(row => {
    const values = row.split(",");
    let obj = {};
    values.forEach((value, index) => {
      obj[headers[index]] = value.trim();
    });
    return obj;
  });
}

const jsonData = parseCSV(csv);
console.log(jsonData);
```
Output:
```Javascript
[
  { Navn: 'Ola', Alder: '30', By: 'Oslo' },
  { Navn: 'Kari', Alder: '25', By: 'Bergen' }
]
```

## Dykk Dypt:
CSV er gammelt, tatt i bruk rundt 1970-tallet med IBM Fortran (en programmeringsspråk). Alternativer til CSV inkluderer JSON og XML, som begge bærer data på en mer strukturert måte. CSV sliter med å representere komplekse datastrukturen og støtter ikke data typer direkte - alt er tekst.

## Se Også:
- MDN Web Docs om `fetch`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- Node.js File System modul, for å lese fra og skrive til filer: https://nodejs.org/api/fs.html
- CSV-specen: https://tools.ietf.org/html/rfc4180
