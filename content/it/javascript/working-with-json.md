---
title:                "Lavorare con JSON"
html_title:           "Javascript: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se stai cercando un modo semplice e versatile per scambiare dati tra diverse applicazioni e piattaforme, allora lavorare con JSON potrebbe essere la soluzione perfetta per te. JSON (JavaScript Object Notation) è un formato di scambio di dati leggero e facile da leggere e scrivere, che lo rende ideale per l'utilizzo in applicazioni web e mobile.

## Come Fare

Per utilizzare JSON in JavaScript, è necessario fare uso del metodo `JSON.parse()` per convertire una stringa JSON in un oggetto JavaScript, e del metodo `JSON.stringify()` per convertire un oggetto JavaScript in una stringa JSON. Ecco un esempio:

```
let studente = {
  nome: "Maria",
  cognome: "Rossi",
  età: 21,
  corsi: ["Matematica", "Storia", "Letteratura"]
};

let studenteJSON = JSON.stringify(studente);
console.log(studenteJSON);
```

Questo codice produrrà l'output seguente:

```
{"nome":"Maria","cognome":"Rossi","età":21,"corsi":["Matematica","Storia","Letteratura"]}
```

Per accedere ai dati all'interno di un oggetto JSON, è possibile utilizzare la notazione a punti o la notazione a parentesi quadrate. Ad esempio:

```
console.log(studente.nome);
console.log(studente["cognome"]);
```

Entrambe le istruzioni di codice produrranno l'output "Maria" e "Rossi".

## Approfondimento

Per ulteriori informazioni su come lavorare con JSON in JavaScript, è possibile esplorare le diverse librerie e framework disponibili, come ad esempio `JSON2` e `jQuery`. Inoltre, è sempre consigliabile familiarizzare con il formato della sintassi di JSON, che è simile alla combinazione di un array JavaScript e un oggetto JavaScript.

## Vedi Anche

- [Introduzione a JSON](https://developer.mozilla.org/it/docs/Learn/JavaScript/Objects/JSON)
- [Manipolazione di JSON in JavaScript](https://www.w3schools.com/js/js_json_intro.asp)
- [Libreria JSON2](https://github.com/douglascrockford/JSON-js)
- [Libreria jQuery](https://api.jquery.com/jquery.getjson/)