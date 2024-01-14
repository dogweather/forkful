---
title:                "Javascript: Ricerca e sostituzione di testo"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Perché

La sostituzione del testo è un'operazione fondamentale nella programmazione JavaScript. È uno strumento utile per correggere errori di ortografia, cambiare nomi di variabili o semplicemente aggiornare testi statici nei nostri progetti. Imparare a farlo correttamente ci permette di risparmiare tempo e rendere il nostro codice più efficiente.

# Come Fare

Per eseguire la sostituzione del testo in JavaScript, dobbiamo utilizzare il metodo `replace()`.
Questo metodo accetta due parametri: il primo è il testo da sostituire e il secondo è il nuovo testo che deve sostituirlo.
Di seguito un esempio di come sostituire la parola "ciao" con "salve" in una stringa:

```Javascript
let testo = "ciao mondo";
let nuovoTesto = testo.replace("ciao", "salve");
console.log(nuovoTesto); // output: "salve mondo"
```

Possiamo anche utilizzare espressioni regolari per sostituire testi che rispettano un determinato pattern. Ad esempio, se volessimo sostituire tutte le vocali in una stringa con l'asterisco "*" possiamo utilizzare il seguente codice:

```Javascript
let testo = "ciao mondo";
let nuovoTesto = testo.replace(/[aeiou]/g, "*");
console.log(nuovoTesto); // output: "c**o m*nd*"
```

Nota che abbiamo utilizzato il modificatore `g` per eseguire la sostituzione su tutte le occorrenze della stringa.

# Deep Dive

Il metodo `replace()` è in realtà molto più potente di quello che abbiamo visto finora. Possiamo anche passare una funzione come secondo parametro, la quale ci permette di eseguire operazioni più complesse sulla stringa da sostituire.
Ad esempio, possiamo utilizzare la funzione per convertire tutte le lettere di una stringa in maiuscolo prima di sostituire il testo:

```Javascript
let testo = "ciao mondo";
let nuovoTesto = testo.replace(/[a-z]/g, (match) => match.toUpperCase());
console.log(nuovoTesto); // output: "CIAO MONDO"
```

Come possiamo vedere, il metodo `replace()` ci offre molte possibilità di personalizzazione nella sostituzione del testo.

# Vedi Anche

- [Documentazione del metodo replace() su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Guida alla sostituzione del testo su TutorialsPoint](https://www.tutorialspoint.com/javascript/javascript_string_replace.htm)
- [Esercizi pratici sulla sostituzione del testo su Javascript.info](https://javascript.info/replace)