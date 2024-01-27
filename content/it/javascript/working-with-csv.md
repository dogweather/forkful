---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con i CSV, ovvero file Comma-Separated Values, significa manipolare dati in un formato testuale semplice. I programmatori lo fanno per importare, esportare, e trasformare dati in modo compatibile con diversi sistemi.

## How to:
Eseguiamo operazioni CSV in JavaScript. Prima, parsiamo un CSV; dopo, lo convertiamo in JSON.

```Javascript
const csv = `name,age
Mario,30
Luca,24`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");

    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }

    result.push(obj);
  }

  return result; // Array di oggetti
}

const data = parseCSV(csv);
console.log(data);
```

Output:
```Javascript
[ { name: 'Mario', age: '30' }, { name: 'Luca', age: '24' } ]
```

## Deep Dive
CSV esiste da prima dei personal computer; era usato in mainframe. Alternativamente, XML e JSON sono usati per dati strutturati, ma sono più verbosi. Nel parse CSV, considera i campi con virgole o nuove righe all'interno delle citazioni e usa librerie come `PapaParse` per casi complessi.

## See Also
- [MDN Web Docs: Working with text](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Text_formatting)
- [PapaParse](https://www.papaparse.com/)
- [CSV on Wikipedia](https://it.wikipedia.org/wiki/Comma-separated_values)
