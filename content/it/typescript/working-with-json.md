---
title:                "Lavorare con json"
html_title:           "TypeScript: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-json.md"
---

{{< edit_this_page >}}

Cosa & perché?:

Lavorare con JSON è molto comune in programmazione e si riferisce semplicemente al formato di dati JavaScript Object Notation. I programmatori spesso lavorano con JSON perché è un formato di dati leggibile sia per gli umani che per le macchine, e può essere facilmente integrato con altre tecnologie come JavaScript e server REST.

Come fare:

Ecco alcuni esempi di come lavorare con JSON in TypeScript:

```TypeScript
// Creare un oggetto JSON con le proprietà "nome" e "età"
const persona = {
  nome: "Marco",
  eta: 25
};

// Convertire l'oggetto JSON in una stringa utilizzando il metodo JSON.stringify
const personaStringa = JSON.stringify(persona);
console.log(personaStringa);  // output: '{"name": "Marco", "age": 25}'

// Convertire una stringa JSON in un oggetto utilizzando il metodo JSON.parse
const oggettoPersona = JSON.parse(personaStringa);
console.log(oggettoPersona.nome);  // output: 'Marco'
console.log(oggettoPersona.eta);   // output: 25
```

Deep Dive:

Il formato JSON è stato creato per rendere più facile lo scambio di dati tra client e server. È stato sviluppato da Douglas Crockford negli anni '90 e ha guadagnato popolarità grazie al crescente utilizzo di API REST. Sebbene inizialmente fosse utilizzato principalmente con JavaScript, oggi è supportato da molte altre tecnologie come essere integrato in qualsiasi linguaggio di programmazione. Un'alternativa comune per memorizzare dati strutturati è l'uso di database relazionali, ma JSON offre maggiore flessibilità e facile accesso tramite chiamate API.

Vedi anche:

- Documentazione di TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-1.html
- Specifiche del formato JSON: https://www.json.org/json-en.html