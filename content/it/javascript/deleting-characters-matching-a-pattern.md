---
title:    "Javascript: Eliminazione di caratteri corrispondenti a un pattern"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché

Eliminare caratteri che corrispondono a un determinato modello è una pratica comune nella programmazione per la pulizia dei dati o per la manipolazione delle stringhe. Può essere utile quando si desidera rimuovere tutte le occorrenze di un determinato carattere o quando si vuole eliminare una parte specifica di una stringa.

## Come fare

Per eliminare i caratteri che corrispondono a un modello in Javascript, si può utilizzare la funzione `replace()` abbinata a un'espressione regolare (regex). Ad esempio, se si vuole eliminare tutte le vocali da una stringa, si può utilizzare il seguente codice:

```Javascript
const stringa = "Ciao mondo!";
const nuovoStringa = stringa.replace(/[aeiou]/gi, "");
console.log(nuovoStringa); //C mnd!
```

Nell'esempio sopra, stiamo utilizzando la `replace()` per sostituire tutti i caratteri che corrispondono al pattern `[aeiou]` (cioè tutte le vocali) con una stringa vuota. L'opzione `g` significa che la ricerca deve essere globale e `i` indica che la ricerca è case-insensitive (non fa distinzione tra maiuscole e minuscole).

Questo è solo un esempio semplice, ma si possono creare espressioni regolari più complesse per eliminare caratteri in base a vari modelli.

## Approfondimento

Le espressioni regolari sono molto potenti e flessibili, ma possono anche essere complicate da comprendere. Se vuoi approfondire le tue conoscenze, ci sono numerosi tutorial e risorse online disponibili. Inoltre, puoi anche esercitarti utilizzando siti come RegExr o provare gli esempi su CodePen.

## Vedi anche

- [MDN - RegExp](https://developer.mozilla.org/it/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr](https://regexr.com/)
- [CodePen](https://codepen.io/)