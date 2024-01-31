---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
JSON, acronimo di JavaScript Object Notation, è un formato leggero per scambiare dati. I programmatori lo usano per la sua semplicità nel salvare oggetti complessi e nella comunicazione tra server e client web.

## How to: (Come Fare:)
```TypeScript
// Definizione di un oggetto JavaScript
const user = {
  name: "Luca",
  age: 30,
  isProgrammer: true
};

// Convertire l'oggetto JavaScript in una stringa JSON
const userJson = JSON.stringify(user);
console.log(userJson); // Output: {"name":"Luca","age":30,"isProgrammer":true}

// Convertire la stringa JSON di nuovo in un oggetto JavaScript
const userObj = JSON.parse(userJson);
console.log(userObj); // Output: { name: 'Luca', age: 30, isProgrammer: true }
```

## Deep Dive (Approfondimento)
JSON è nato negli anni 2000 come alternativa più leggera e facile da leggere rispetto all'XML. Anche se "JavaScript" è nel nome, è indipendente dal linguaggio e può essere usato in molti contesti di programmazione. TypeScript, essendo un superset di JavaScript, gestisce JSON nativamente, ma aggiunge tipizzazioni per lavorare con i dati in modo più sicuro.

## See Also (Vedi Anche)
- [JSON.org](https://www.json.org/json-en.html): La risorsa ufficiale per sapere tutto su JSON.
- [MDN Web Docs on JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON): Documentazione MDN per approfondire le API di JSON in JavaScript.
- [TypeScript Handbook](https://www.typescriptlang.org/docs/): Per comprendere meglio come TypeScript estende JavaScript, incluso il lavoro con JSON.
