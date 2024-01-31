---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Vi jobbar med CSV-filer (Comma-Separated Values) för att enkelt hantera tabulär data. Programmerare använder CSV för dess enkelhet och kompatibilitet med kalkylprogram och databaser.

## How to:
Du kan läsa och skriva CSV med JavaScript. Här använder vi 'PapaParse', ett populärt bibliotek.

Installera med npm:
```bash
npm install papaparse
```

Läs en CSV:
```javascript
const Papa = require('papaparse');
const fs = require('fs');

const csvFile = fs.readFileSync('dinFil.csv', 'utf8');

Papa.parse(csvFile, {
  complete: function(results) {
    console.log(results.data);
  }
});
```

Skriv en CSV:
```javascript
const { Parser } = require('papaparse');
const fs = require('fs');

const data = [
  { name: "Anna", age: 28 },
  { name: "Lars", age: 35 }
];

const csv = new Parser({fields: ["name", "age"]}).parse(data);

fs.writeFileSync('utFil.csv', csv);
```

Resultat:
```
name,age
Anna,28
Lars,35
```

## Deep Dive
CSV-formatet har använts sedan tidigt 1970-tal. Alternativ inkluderar JSON och XML, men CSV är fortfarande populärt för sin enkelhet. Implementationen i JavaScript kan kräva tredjepartsbibliotek som 'PapaParse' för parsing och serialization då JS inte har inbyggt stöd för CSV-format.

## See Also
- PapaParse dokumentation: [PapaParse Documentation](https://www.papaparse.com/docs)
- Mozilla Developer Network, Arbeta med textfiler: [MDN Working with Text Files](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications#Example_Reading_a_text_file)
