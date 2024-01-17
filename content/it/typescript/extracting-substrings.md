---
title:                "Estrarre sottostringhe"
html_title:           "TypeScript: Estrarre sottostringhe"
simple_title:         "Estrarre sottostringhe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il processo di estrarre sottostringhe (o "substring") è una pratica comune tra i programmatori. Si tratta di prelevare una sottostringa più piccola da una stringa più grande, in base a criteri specifici. Ad esempio, potresti voler estrarre solo i primi 5 caratteri di una parola.

In generale, gli sviluppatori utilizzano la tecnica delle sottostringhe per poter manipolare le stringhe in modo più flessibile e preciso, adattandole alle proprie esigenze.

## Come:

Di seguito ci sono alcuni esempi di come estrarre sottostringhe in TypeScript:

```TypeScript
let str = 'Benvenuti!';
let subs = str.substring(0, 5); // estrae i primi 5 caratteri
console.log(subs); // output: Benve
```
```TypeScript
let str = '123456';
let subs = str.substr(-3); // estrae gli ultimi 3 caratteri
console.log(subs); // output: 456
```

```TypeScript
let str = 'Buon compleanno';
let subs = str.slice(4, 13); // estrae i caratteri dalla posizione 4 alla 12
console.log(subs); // output: compleanno
```

Oltre alle funzioni `substring()`, `substr()` e `slice()`, TypeScript offre anche il metodo `charAt()` che permette di estrarre un singolo carattere da una stringa in base alla sua posizione.

## Approfondimento:

L'operazione di estrarre sottostringhe ha radici nelle prime versioni di linguaggi di programmazione come il Fortran e il Cobol. In quei tempi, le stringhe erano trattate come array di caratteri e per accedere a una parte specifica si usavano tecniche di indicizzazione simili a quelle utilizzate oggi con le sottostringhe.

Un'alternativa alla tecnica delle sottostringhe è l'utilizzo di espressioni regolari, che permettono di manipolare le stringhe utilizzando pattern di ricerca e sostituzione.

Per quanto riguarda l'implementazione in TypeScript, le funzioni `substring()`, `substr()` e `slice()` si comportano in modo simile alle loro controparti in JavaScript, con alcune differenze riguardo ai valori di input.

## Vedi anche:

- La documentazione ufficiale di TypeScript: https://www.typescriptlang.org/docs/
- Una guida dettagliata all'utilizzo delle sottostringhe in TypeScript: https://www.tutorialspoint.com/typescript/typescript_strings.htm
- Un confronto tra le funzioni di manipolazione delle stringhe in TypeScript: https://www.javascripttuts.com/typescript-string-functions/