---
title:                "TypeScript: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Lavorare con JSON è fondamentale per lo sviluppo di applicazioni moderne. JSON, acronimo di JavaScript Object Notation, è un formato di scambio dati leggero e facile da leggere e scrivere. Grazie alla sua ampia diffusione, la conoscenza di come trattare i dati in formato JSON è un'imponente abilità per ogni sviluppatore di TypeScript.

## Come fare

In TypeScript, è possibile manipolare oggetti JSON in modo diretto grazie all'utilizzo delle interfacce. Ad esempio, per convertire un oggetto JavaScript in formato JSON si può utilizzare il metodo "JSON.stringify". Ecco un esempio di codice:

```TypeScript
const persona = {
  nome: 'Mario',
  cognome: 'Rossi',
  età: 30
};

const personaJSON = JSON.stringify(persona);
console.log(personaJSON);
```

Questo produrrà l'output: `{"nome":"Mario","cognome":"Rossi","età":30}`

Per effettuare l'operazione inversa, ovvero convertire una stringa JSON in un oggetto JavaScript, si può utilizzare il metodo "JSON.parse". Ecco un altro esempio di codice:

```TypeScript
const animaleJSON = '{"nome":"Fido","specie":"cane","età":5}';
const animale = JSON.parse(animaleJSON);
console.log(animale.nome);
```

L'output sarà: `Fido`

## Approfondimento

Se si stanno elaborando grandi quantità di dati in formato JSON, è possibile utilizzare la libreria esterna "json-bigint" per gestire numeri interi con una precisione maggiore rispetto alla libreria standard "JSON". Inoltre, è possibile sfruttare al massimo le interfacce di TypeScript per rendere più pulito e più sicuro il codice che manipola dati JSON.

## Vedi anche

- Documentazione ufficiale di TypeScript su JSON: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html
- Introduzione a JSON su MDN: https://developer.mozilla.org/it/docs/Learn/JavaScript/Objects/JSON