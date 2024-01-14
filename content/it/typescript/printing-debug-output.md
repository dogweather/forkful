---
title:                "TypeScript: Stampa dell'output di debug"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare output di debug nella programmazione TypeScript può essere utile per comprendere meglio il comportamento del codice, individuare errori e migliorare le prestazioni.

## Come Fare

Per stampare output di debug in TypeScript, possiamo utilizzare il metodo `console.log()` con il valore o la variabile che vogliamo stampare come argomento. Ecco un esempio:

```TypeScript
let numero = 5;
console.log(numero);
```

Questo produrrà l'output `5` nella console del browser o del terminale in cui viene eseguito il codice.

## Approfondimento

Stampare output di debug è un modo per avere una visione più approfondita del nostro codice durante l'esecuzione. Possiamo anche utilizzare `console.log()` per stampare valori di variabili in punti specifici del nostro codice, ad esempio per controllare il valore di una variabile all'interno di un ciclo. Inoltre, possiamo utilizzare formattazione di stringhe e variabili multiple all'interno di `console.log()`, come nel seguente esempio:

```TypeScript
let nome = "Maria";
let cognome = "Rossi";
console.log(`Il nome completo è ${nome} ${cognome}`);
```

Questo produrrà l'output `Il nome completo è Maria Rossi`.

## Vedi Anche

- [Documentazione di console in TypeScript](https://www.typescriptlang.org/docs/handbook/console.html)
- [Come fare il debugging in TypeScript con VS Code](https://code.visualstudio.com/docs/nodejs/nodejs-debugging)
- [Utilizzo di console.log() per il debugging in TypeScript](https://www.digitalocean.com/community/tutorials/typescript-debugging-concepts)