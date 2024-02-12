---
title:                "Lavorare con JSON"
aliases:
- /it/javascript/working-with-json.md
date:                  2024-02-03T19:22:57.287210-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?

JSON (JavaScript Object Notation) è un formato leggero per lo scambio di dati, facile da leggere e scrivere per gli umani e da analizzare e generare per le macchine. I programmatori lo utilizzano per memorizzare e trasportare dati nelle applicazioni web, rendendolo la colonna vertebrale della comunicazione moderna tra API e servizi web.

## Come fare:

### Analizzare JSON
Per convertire una stringa JSON in un oggetto JavaScript, utilizzare `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Output: John
```

### Trasformare Oggetti JavaScript in Stringhe JSON
Per convertire un oggetto JavaScript di nuovo in una stringa JSON, utilizzare `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Output: {"name":"Jane","age":25,"city":"London"}
```

### Lavorare con i File in Node.js
Per leggere un file JSON e convertirlo in un oggetto in un ambiente Node.js, è possibile utilizzare il modulo `fs`. Questo esempio presupone che si abbia un file denominato `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

Per scrivere un oggetto in un file JSON:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('Dati scritti sul file');
});
```

### Librerie di Terze Parti
Per operazioni JSON complesse, framework e librerie come `lodash` possono semplificare le attività, ma per le operazioni di base, le funzioni JavaScript native sono spesso sufficienti. Per applicazioni su larga scala o critiche per le prestazioni, è possibile considerare librerie come `fast-json-stringify` per una serializzazione JSON più veloce o `json5` per l'analisi e la serializzazione utilizzando un formato JSON più flessibile.

Analisi con `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Output: John
```

Questi esempi coprono le operazioni di base con JSON in JavaScript, perfetti per i principianti in transizione da altri linguaggi e desiderosi di gestire dati nelle applicazioni web in modo efficiente.
