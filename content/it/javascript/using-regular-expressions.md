---
title:                "Utilizzare le espressioni regolari"
html_title:           "Javascript: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Che cosa è e perché?

Le espressioni regolari, anche conosciute come regex, sono uno strumento utilizzato dai programmatori per cercare e manipolare testo all'interno di una stringa. Sono spesso utilizzate per convalidare input utente, trovare corrispondenze in un documento di testo o per la ricerca e sostituzione di parole o frasi specifiche.

## Come fare:

Usare le espressioni regolari in JavaScript è semplice e potente. Basta utilizzare il costruttore `RegExp()` per creare un nuovo oggetto regex e quindi utilizzarlo con le funzioni disponibili nel linguaggio. Ad esempio, per trovare tutte le parole che iniziano con la lettera "a" in una stringa, si può usare il seguente codice:

```javascript
let stringa = "Amore, amicizia, albero, acqua";
let regex = new RegExp(/\ba\w*/g);
let risultati = stringa.match(regex);
console.log(risultati); // ["Amore", "amicizia", "albero"]
```

Nell'esempio sopra, il costrutto regex /\ba\w*/g cerca le parole che iniziano con "a" seguite da qualsiasi altra lettera, incluse le parole con spazi. Utilizzando la funzione `match()` con l'oggetto regex, verranno trovate e restituite tutte le corrispondenze nella stringa.

È anche possibile utilizzare le regex per validare input utente. Ad esempio, se si vuole che un utente inserisca un indirizzo email valido, si può utilizzare il seguente codice:

```javascript
function validaEmail(email) {
  return /^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$/.test(email);
}

let email = "esempio@esempio.com";
console.log(validaEmail(email)); // true
```

In questo caso, la regex utilizzata controlla che l'indirizzo email contenga il simbolo "@", un dominio valido e una lunghezza del dominio di 2-4 caratteri.

## Approfondimento:

Le espressioni regolari esistono da molto tempo e sono state introdotte nei primi anni '50. Sono state originariamente utilizzate nei sistemi di gestione di testo, ma ora sono supportate da molti linguaggi di programmazione, compresi JavaScript, PHP, Python e molti altri.

Ci sono anche alternative alle espressioni regolari come le "stringhe di ricerca" in linguaggi come Python o le funzioni di manipolazione del testo in JavaScript. Tuttavia, l'utilizzo delle espressioni regolari offre una maggiore precisione e velocità nelle operazioni di ricerca e manipolazione del testo.

Per quanto riguarda l'implementazione in JavaScript, ci sono alcuni metodi utili che possono essere utilizzati con le regex, tra cui `test()`, `exec()`, `match()`, `search()`, `replace()` e `split()`. Ognuno di questi metodi può essere utilizzato per eseguire operazioni diverse con regex su una stringa.

## Vedi anche:

Per altre informazioni e esempi sull'utilizzo delle espressioni regolari in JavaScript, consultare la documentazione ufficiale di MDN: https://developer.mozilla.org/it/docs/Web/JavaScript/Guide/Regular_Expressions.