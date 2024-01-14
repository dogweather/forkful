---
title:    "Javascript: Utilizzando le espressioni regolari"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché utilizzare le espressioni regolari in Javascript?

Le espressioni regolari sono un potente strumento che permette di cercare e manipolare stringhe di testo in modo efficiente ed elegante. In Javascript, sono supportate nativamente e possono essere utilizzate in una varietà di situazioni, come la validazione dei dati degli utenti, la ricerca di parole chiave in un testo o la formattazione di una stringa secondo uno schema specifico.

## Come utilizzare le espressioni regolari in Javascript

Per utilizzare le espressioni regolari in un progetto Javascript, è necessario utilizzare l'oggetto RegExp che rappresenta una espressione regolare. Esistono due modi per creare un'istanza di RegExp: utilizzando la sintassi letterale, che prevede l'utilizzo di due slash (`/`) per delimitare l'espressione, o il costruttore RegExp. Vediamo un esempio di entrambi i metodi:

```
// utilizzando la sintassi letterale
let regExp = /esempio/i;

// utilizzando il costruttore
let regExp = new RegExp("esempio", "i");
```

Nell'esempio sopra, abbiamo creato un'istanza di RegExp per cercare la parola "esempio" all'interno di una stringa, ignorando la differenza tra maiuscole e minuscole grazie all'utilizzo della flag `i`. Ma come utilizziamo questa espressione regolare?

Uno dei metodi più comuni dell'oggetto RegExp è `test()`, che accetta una stringa come argomento e restituisce `true` se la stringa corrisponde all'espressione regolare, altrimenti `false`. Vediamo un esempio:

```
let regExp = /esempio/i;
let str = "Questo è un esempio di utilizzo delle espressioni regolari in Javascript";
console.log(regExp.test(str)); // output: true
```

Possiamo anche utilizzare il metodo `search()` che restituisce l'indice della prima corrispondenza all'interno della stringa, oppure `match()` che restituisce un array delle corrispondenze trovate.

## Approfondimento sulle espressioni regolari in Javascript

Le espressioni regolari in Javascript supportano molte funzionalità e sono composte da una serie di caratteri speciali che hanno significati specifici. Ad esempio, il punto `.` rappresenta qualsiasi carattere, mentre il carattere `^` indica l'inizio di una stringa. Inoltre, le espressioni regolari in Javascript permettono di utilizzare i quantificatori, che indicano il numero di ripetizioni di un certo carattere o gruppo di caratteri. Ad esempio, l'asterisco `*` indica che il carattere precedente può essere ripetuto 0 o più volte, mentre il più `+` indica che deve essere ripetuto almeno una volta.

È importante sottolineare che le espressioni regolari non sono specifiche di Javascript, ma sono utilizzate in molti altri linguaggi di programmazione e hanno una sintassi simile. Quindi, una volta imparata la sintassi di base, sarà più facile utilizzarle anche in altri contesti.

## Vedi anche

- [Documentazione MDN sulle espressioni regolari in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/RegExp)
- [Tutorial su come utilizzare le espressioni regolari in Javascript](https://www.w3schools.com/js/js_regexp.asp)
- [Lista di espressioni regolari utili per la validazione dei dati](https://regexlib.com/)