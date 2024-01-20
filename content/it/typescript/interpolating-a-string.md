---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
L'interpolazione delle stringhe è un processo che permette di inserire variabili o espressioni all'interno di una stringa. Lo facciamo per rendere il nostro codice più leggibile e conciso.

## Come fare:
Ecco come possiamo fare l'interpolazione delle stringhe in TypeScript:

```TypeScript
let nome = "Mario";
let eta = 30;
let salute = `Ciao ${nome}, hai ${eta} anni.`;
console.log(salute); // Output: Ciao Mario, hai 30 anni.
```

In questo esempio, abbiamo inserito le variabili `nome` ed `eta` all'interno della stringa `salute` utilizzando il simbolo del dollaro seguito da parentesi graffe `${var_name}`.

## Deep Dive
L'interpolazione delle stringhe non è un concetto nuovo. Infatti, è stato popolare in molte altre lingue, come Python e Ruby, prima di diventare una funzionalità standard in ES6 (la versione di JavaScript su cui si basa TypeScript). 

Come alternativa all'interpolazione delle stringhe, potevamo concatenare le stringhe utilizzando l'operatore `+`. Tuttavia, l'interpolazione delle stringhe è generalmente più leggibile e meno propensa agli errori.

Quando interpoliamo una stringa, TypeScript sostituisce l'espressione all'interno delle parentesi graffe con il suo valore corrente, poi converte tutto in una stringa. Se l'espressione all'interno delle parentesi graffe ritorna un oggetto, TypeScript chiama automaticamente il metodo `toString()` dell'oggetto.

## Vedi Anche
Per ulteriori informazioni sull'interpolazione delle stringhe in TypeScript, potete consultare le seguenti risorse:

- [Un tutorial su YouTube sull'interpolazione delle stringhe in TypeScript](https://www.youtube.com/watch?v=2Nt8gQD4g5A)