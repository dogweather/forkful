---
title:    "TypeScript: Stampa dell'output di debug"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Debuggare il proprio codice è una parte essenziale del processo di sviluppo software. Stampare output di debug aiuta i programmatori a identificare e risolvere eventuali errori e problemi nel loro codice. In questo articolo, esploreremo come stampare output di debug utilizzando TypeScript.

## Come fare

Per stampare output di debug in TypeScript, possiamo utilizzare la funzione `console.log()`. Questa funzione prende come argomento una stringa o un valore da stampare e lo visualizza nella console. Vediamo un esempio:

```TypeScript
let nome = "Mario";
let età = 25;

console.log(`Ciao, mi chiamo ${nome} e ho ${età} anni.`);
```

Questo codice stampa l'output "Ciao, mi chiamo Mario e ho 25 anni." nella console. Possiamo anche stampare più di un valore alla volta passandoli come argomenti separati nella funzione `console.log()`, ad esempio:

```TypeScript
let num1 = 10;
let num2 = 5;

console.log("Il risultato è:", num1 + num2);
```

Questo codice stampa l'output "Il risultato è: 15" nella console.

## Approfondimento

Insieme alla funzione `console.log()`, TypeScript offre anche altre opzioni per stampare output di debug. Una di queste è `console.info()`, che viene usata per stampare messaggi informativi. Possiamo anche utilizzare `console.warn()` per avvertire gli sviluppatori di eventuali problemi nel codice e `console.error()` per segnalare errori gravi.

Per rendere i messaggi di debug più utili, possiamo anche utilizzare la funzione `console.assert()`, che verifica se una condizione è vera e se non lo è, stampa un messaggio di errore nella console. Vediamo un esempio:

```TypeScript
let num = 15;

console.assert(num < 10, "Il numero deve essere minore di 10");
```

Se la condizione è vera, il messaggio di errore non verrà stampato. Ma se la condizione è falsa, vedremo "Assertion failed: Il numero deve essere minore di 10" nella console.

Inoltre, TypeScript supporta l'uso delle stringhe di caratteri speciali per formattare gli output di debug. Possiamo utilizzare `\n` per andare a capo e `\t` per tabulare il testo. Esempio:

```TypeScript
console.log("Prima riga\nSeconda riga\nTerza riga");
```

L'output sarà:

```
Prima riga
Seconda riga
Terza riga
```

## Vedi anche

- [Documentazione ufficiale di TypeScript](https://www.typescriptlang.org/docs/home.html)
- [Tutorial di TypeScript su Udemy](https://www.udemy.com/course/typescript-tutorial/)
- [Debugging in TypeScript: Tips and Tricks](https://blog.bitsrc.io/debugging-in-typescript-tips-and-tricks-82737f5efb8c) (in inglese)