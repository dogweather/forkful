---
title:    "TypeScript: Eliminazione di caratteri corrispondenti a un modello"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché:

A volte può essere necessario eliminare i caratteri che corrispondono ad un certo pattern in un programma TypeScript. Questo potrebbe essere utile per esempio quando si vuole rimuovere gli spazi bianchi da una stringa o quando si deve filtrare un input utente.

## Come fare:

Per eliminare i caratteri che corrispondono ad un pattern, possiamo utilizzare il metodo `replace()` della classe `string` di TypeScript. Questo metodo accetta due parametri: il primo è il pattern da cercare e il secondo è il carattere con cui sostituire quello trovato. Vediamo un esempio pratico di come utilizzarlo:

```TypeScript
let str = "Hello, world!";
let newStr = str.replace(/l/g, ""); //elimina tutti i caratteri "l"
console.log(newStr); //stampa "Heo, word!"
```

Come possiamo vedere, il metodo `replace()` restituisce una nuova stringa con i caratteri eliminati. In questo caso, abbiamo utilizzato una espressione regolare come pattern, ma è possibile utilizzare anche una stringa semplice.

## Approfondimento:

Il metodo `replace()` di TypeScript utilizza le espressioni regolari per cercare il pattern specificato nella stringa. Le espressioni regolari sono degli schemi di ricerca predefiniti che permettono di trovare stringhe che corrispondono ad un certo formato. Ad esempio, nel nostro esempio abbiamo utilizzato la `g` dopo il pattern per indicare che vogliamo eliminare tutti i caratteri che corrispondono, non solo il primo trovato. 

Inoltre, è importante ricordare che il metodo `replace()` non modifica la stringa originale, ma ne crea una nuova. Se vogliamo modificare la stringa originale, dobbiamo assegnare il risultato del metodo ad essa.

## Vedi anche:

- [Documentazione ufficiale di TypeScript sul metodo `replace()`](https://www.typescriptlang.org/docs/handbook/enums.html)
- [Tutorial sulle espressioni regolari in TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-typescript)