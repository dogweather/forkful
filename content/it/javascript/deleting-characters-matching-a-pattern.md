---
title:    "Javascript: Eliminazione di caratteri corrispondenti a un modello."
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono diverse ragioni per cui si potrebbe voler cancellare dei caratteri che corrispondono a un certo pattern in Javascript. Ad esempio, potresti voler rimuovere tutte le vocali da una stringa oppure eliminare tutti i caratteri che non sono numeri. Questo può essere utile quando si lavora con dati grezzi da un database o quando si desidera pulire una stringa prima di elaborarla in modo più approfondito.

## Come fare

Per eliminare i caratteri che corrispondono a un certo pattern in Javascript, è possibile utilizzare il metodo `replace()` associato alle espressioni regolari. Ecco un esempio di codice:

```Javascript
// Creazione di una stringa di esempio
let stringa = "Questo è un esempio di stringa con alcune vocali.";
// Utilizzo del metodo replace() con un'espressione regolare per eliminare le vocali
let stringaPulita = stringa.replace(/[aeiou]/gi, "");
// Stampiamo il risultato
console.log(stringaPulita); // Stampa: "Qst è n msp d strng cn lcn vcl."

```

In questo esempio, abbiamo creato una stringa di esempio e utilizzato il metodo `replace()` con l'espressione regolare `/[aeiou]/gi` per sostituire tutte le vocali, indipendentemente dal loro caso, con una stringa vuota.

Un altro esempio comune è quello di eliminare tutti i caratteri che non sono numeri da una stringa:

```Javascript
// Creazione di una stringa di esempio
let stringa = "Questo è un esempio di stringa con alcuni numeri 12345.";
// Utilizzo del metodo replace() con un'espressione regolare per eliminare i caratteri che non sono numeri
let stringaPulita = stringa.replace(/[^0-9]/g, "");
// Stampiamo il risultato
console.log(stringaPulita); // Stampa: "12345"

```

In questo caso, abbiamo utilizzato l'espressione regolare `/[^0-9]/g` per sostituire tutti i caratteri che non sono numeri con una stringa vuota, ottenendo così solo i numeri rimanenti.

## Approfondimento

È possibile utilizzare le espressioni regolari in modo molto più complesso per eliminare i caratteri desiderati dalle stringhe in Javascript. Le espressioni regolari sono una potente e flessibile strumento per la manipolazione delle stringhe e possono essere utilizzate in molte altre situazioni oltre all'eliminazione di caratteri specifici.

Per ulteriori informazioni sulle espressioni regolari e su come utilizzarle efficacemente in Javascript, consiglio di consultare questi articoli:

- [Guida rapida alle espressioni regolari in Javascript](https://www.javascripttutorial.net/regular-expressions-in-javascript/)
- [Manuale di riferimento su JavaScript: Espressioni regolari](https://developer.mozilla.org/it/docs/Web/JavaScript/Guide/Regular_Expressions)

## Vedi anche

- [Mozila Developer Network: Utilizzo delle espressioni regolari in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex101: Uno strumento online per testare ed esplorare le espressioni regolari](https://regex101.com/)