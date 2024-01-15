---
title:                "Cercare e sostituire il testo"
html_title:           "TypeScript: Cercare e sostituire il testo"
simple_title:         "Cercare e sostituire il testo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, mentre stiamo scrivendo codice, dobbiamo effettuare delle modifiche o correzioni su una porzione di testo. Ciò può essere molto tedioso se si tratta di una stringa di testo molto lunga o se la correzione deve essere effettuata su più occorrenze. In questo caso, l'utilizzo di una funzione di ricerca e sostituzione può risultare molto utile ed efficiente.

## Come fare

Per utilizzare la funzione di ricerca e sostituzione in TypeScript, dobbiamo utilizzare il metodo `replace()` su una stringa di testo. Questo metodo accetta due parametri: il testo da cercare e il testo con cui sostituirlo. Di seguito un esempio di utilizzo:

```TypeScript
let stringa = "Questo è un esempio di testo";
stringa = stringa.replace("esempio", "esempio modificato");

console.log(stringa); // Questo è un esempio modificato di testo
```

In questo esempio, la parola "esempio" è stata cercata all'interno della stringa e sostituita con la parola "esempio modificato". Il nuovo valore della stringa è stato assegnato alla stessa variabile, in modo da sovrascrivere il valore originale.

Possiamo anche utilizzare espressioni regolari per effettuare una ricerca e sostituzione più avanzata. Ad esempio, se volessimo sostituire tutte le vocali con un asterisco, potremmo utilizzare il seguente codice:

```TypeScript
let stringa = "Questo è un esempio di testo";
stringa = stringa.replace(/[aeiou]/gi, "*");

console.log(stringa); // Q**** è *n *mp** di t*st*
```

In questo caso, abbiamo utilizzato l'espressione regolare `/[aeiou]/gi` per selezionare tutte le vocali senza distinzione tra maiuscole e minuscole, e le abbiamo sostituite con l'asterisco.

## Approfondimento

La funzione di ricerca e sostituzione in TypeScript può essere utilizzata anche con delle callback per effettuare una sostituzione dinamica. Ad esempio:

```TypeScript
let stringa = "Corsa mattutina";
stringa = stringa.replace(/(corsa) (mattutina)/i, (match, p1, p2) => p1.toUpperCase() + " " + p2.toLowerCase());

console.log(stringa); // CORSA mattutina
```

In questo caso, abbiamo utilizzato un'espressione regolare per cercare la parola "corsa" seguita dalla parola "mattutina". Utilizzando una callback, abbiamo trasformato la prima parola in maiuscolo e la seconda in minuscolo.

## Vedi anche

- Documentazione ufficiale su `replace()` in TypeScript: https://www.typescriptlang.org/docs/handbook/basic-types.html#string-replace
- Tutorial sulle espressioni regolari in TypeScript: https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm
- Esempi pratici di utilizzo di `replace()` in TypeScript: https://www.javascripttutorial.net/javascript-string-replace/