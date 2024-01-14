---
title:    "TypeScript: Capitalizzare una stringa"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è un'operazione comune nel mondo della programmazione, poiché può essere utile per formattare correttamente i dati in base ai nostri bisogni. In TypeScript, esistono alcune funzioni che ci permettono di capitalizzare una stringa facilmente, rendendo così il nostro codice più efficiente e leggibile.

## Come fare

Per capitalizzare una stringa in TypeScript, possiamo utilizzare la funzione `toUpperCase()` che viene ereditata dal tipo di dato `string`. Per farlo, seguiamo questi semplici passaggi:

1. Definisci una variabile di tipo `string` contenente la stringa che desideriamo capitalizzare.
2. Utilizza la funzione `toUpperCase()` sulla variabile appena creata.
3. Stampa il risultato a console o assegnalo ad una nuova variabile.

Ecco un codice esempio che mostra come capitalizzare una stringa in TypeScript:

```TypeScript
let nome = "gianni";
let nomeCapitalizzato = nome.toUpperCase();
console.log(nomeCapitalizzato); // Output: "GIANNI"
```

In questo esempio, abbiamo utilizzato la funzione `toUpperCase()` sulla stringa "gianni", ottenendo come risultato la stringa "GIANNI". Questo metodo è molto semplice e veloce, ma non è l'unico modo per capitalizzare una stringa in TypeScript.

Un'altra opzione è utilizzare la funzione `charAt()` insieme alla funzione `toUpperCase()`. Questo ci permette di capitalizzare solo la prima lettera della stringa. Ecco un esempio di come fare:

```TypeScript
let nome = "gianni";
let primaLetteraCapitalizzata = nome.charAt(0).toUpperCase() + nome.slice(1);
console.log(primaLetteraCapitalizzata); // Output: "Gianni"
```

Come puoi vedere, utilizzando la funzione `charAt()` selezioniamo il carattere nella posizione 0, che corrisponde alla prima lettera della stringa. Usando poi `toUpperCase()` e `slice()`, capitalizziamo la prima lettera e uniamo il resto della stringa non modificato. In questo modo, otteniamo la stringa "Gianni".

## Approfondimento

Ci sono altre tecniche per capitalizzare una stringa in TypeScript, ad esempio utilizzando cicli o regex. Inoltre, possiamo creare funzioni personalizzate che prendono in input una stringa e la restituiscono capitalizzata. È importante notare che la funzione `toUpperCase()` converte la stringa in maiuscolo di default. Se desideriamo capitalizzare la stringa in minuscolo, possiamo utilizzare la funzione `toLowerCase()` invece.

## Vedi anche

- [Documentazione ufficiale di TypeScript](https://www.typescriptlang.org/docs/)
- [W3Schools - TypeScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [MDN Web Docs - String methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#Methods)