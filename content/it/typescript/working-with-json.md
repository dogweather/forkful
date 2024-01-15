---
title:                "Programmare con json"
html_title:           "TypeScript: Programmare con json"
simple_title:         "Programmare con json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore in TypeScript, molto probabilmente hai già avuto a che fare con l'utilizzo di dati in formato JSON. Questo formato è molto comune nel mondo della programmazione, soprattutto per lo scambio di dati tra diverse applicazioni. Leggere e manipolare dati in formato JSON è fondamentale per la maggior parte dei progetti di sviluppo, quindi è importante avere una buona comprensione di come funziona.

## Come fare

Per utilizzare i dati JSON in TypeScript, è necessario prima importare il modulo "JSON". Questo modulo fornisce alcune utili funzionalità per convertire i dati JSON in oggetti TypeScript e viceversa. Vediamo un esempio pratico:

```TypeScript
import { JSON } from "json";

let json = JSON.stringify({name: "Marco", age: 25});
console.log(json); // Output: {"name": "Marco", "age": 25}

let object = JSON.parse(json);
console.log(object.name); // Output: Marco
```

In questo esempio, abbiamo importato il modulo JSON e utilizzato le sue funzionalità `stringify` e `parse` per convertire i dati tra formato JSON e oggetto TypeScript. È importante notare che quando si utilizzano oggetti JSON, è necessario fare attenzione alle virgolette e alla sintassi corretta.

## Approfondimento

Mentre il codice mostrato sopra può essere utile per lavori semplici con dati JSON, ci possono essere situazioni in cui è necessario effettuare operazioni più complesse. Ad esempio, supponiamo di avere un file JSON con dati di diversi utenti e vogliamo filtrarli in base all'età. Possiamo utilizzare il metodo `filter` fornito dal modulo JSON per fare ciò:

```TypeScript
let usersJson = [
    {name: "Marco", age: 25},
    {name: "Giulia", age: 20},
    {name: "Luca", age: 30}
];
let users = JSON.parse(usersJson);

let filteredUsers = users.filter(user => user.age > 25);
console.log(filteredUsers); // Output: [{name: "Luca", age: 30}]
```

In questo caso, abbiamo utilizzato il metodo `filter` per ottenere solo gli utenti con un'età superiore a 25 anni. Anche qui, è importante prestare attenzione alla sintassi corretta per ottenere il risultato desiderato.

## Vedi anche 

- [Documentazione ufficiale di TypeScript per il modulo JSON](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html)
- [Tutorial su come utilizzare dati JSON in TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-json-in-typescript)