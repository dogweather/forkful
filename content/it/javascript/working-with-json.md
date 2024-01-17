---
title:                "Lavorare con json"
html_title:           "Javascript: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con JSON significa manipolare e gestire dati in formato JSON (JavaScript Object Notation). I programmatori utilizzano JSON perché è un formato leggero e facile da leggere e scrivere, rendendolo ideale per lo scambio di dati tra applicazioni web.

## Come:
Ecco alcuni esempi di codice per lavorare con JSON in Javascript:

```Javascript
// Creazione di un oggetto JSON
var student = { name: "Mario", age: 20, major: "Computer Science"};

// Conversione di un oggetto JSON in una stringa
var studentString = JSON.stringify(student);
console.log(studentString); // Output: {"name":"Mario","age":20,"major":"Computer Science"}

// Parsing di una stringa JSON in un oggetto
var studentObject = JSON.parse(studentString);
console.log(studentObject.age); // Output: 20
```

## Approfondimento:
JSON è nato come parte di JavaScript, ma adesso è utilizzato in diversi linguaggi di programmazione. Alcune alternative a JSON includono XML e CSV, tuttavia JSON è diventato lo standard de facto per lo scambio di dati nel web. Per lavorare con JSON in modo più avanzato, è possibile utilizzare librerie come jQuery e lodash.

## Vedi anche:
- [Introduzione a JSON](https://www.json.org/json-it.html)
- [Una guida rapida a JSON in Javascript](https://www.w3schools.com/js/js_json_intro.asp)
- [Manipolazione di dati JSON con jQuery](https://api.jquery.com/jquery.getjson/)