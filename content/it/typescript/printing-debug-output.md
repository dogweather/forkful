---
title:                "Stampa di output di debug"
html_title:           "TypeScript: Stampa di output di debug"
simple_title:         "Stampa di output di debug"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perchè

Stampare gli output di debug è un modo efficace per verificare il corretto funzionamento del nostro codice. Ci permette di visualizzare valori, messaggi di errore e altre informazioni utili durante l'esecuzione del programma.

## Come

Per stampare gli output di debug in TypeScript, possiamo utilizzare la funzione `console.log()`. Questa funzione accetta come argomento qualsiasi tipo di dato e lo stampa nella console del nostro browser o della nostra IDE.

```TypeScript
console.log("Hello World!");
// Output: Hello World!

let num = 10;
console.log(num);
// Output: 10

let arr = [1, 2, 3];
console.log(arr);
// Output: [1, 2, 3]

let obj = { name: "John", age: 25 };
console.log(obj);
// Output: { name: "John", age: 25 }
```

Possiamo anche utilizzare la sintassi di template string per stampare messaggi di debug con valori dinamici:

```TypeScript
let name = "John";
console.log(`Hello ${name}!`);
// Output: Hello John!
```

Inoltre, possiamo utilizzare la funzione `console.error()` per stampare un messaggio di errore nella console:

```TypeScript
console.error("Oops, something went wrong!");
// Output: Oops, something went wrong!
```

## Deep Dive

Oltre alla semplice stampa di valori o messaggi di errore, possiamo utilizzare diverse funzioni della console per facilitare il debug dei nostri programmi. Alcune di esse sono:

- `console.info()`: per stampare un messaggio informativo
- `console.warn()`: per stampare un messaggio di avviso
- `console.debug()`: per stampare un messaggio di debug
- `console.trace()`: per stampare lo stack trace dell'errore
- `console.table()`: per stampare un array di oggetti in formato tabellare

Inoltre, possiamo utilizzare la direttiva `DEBUG` nel nostro codice per abilitare o disabilitare la stampa degli output di debug in base all'ambiente in cui il programma viene eseguito.

## Vedi anche

- [Documentazione ufficiale di TypeScript](https://www.typescriptlang.org/docs)
- [Articolo su come utilizzare il debugger in TypeScript](https://medium.com/javascript-in-plain-english/debugging-typescript-projects-with-visual-studio-code-a8d5f50f45c4)
- [Video tutorial su come usare console.log in TypeScript](https://www.youtube.com/watch?v=JpNidkC1Y1U)