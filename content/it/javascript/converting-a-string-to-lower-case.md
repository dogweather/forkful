---
title:                "Javascript: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con i dati in un programma Javascript, è necessario manipolare le stringhe in vari modi. Una delle operazioni più comuni è convertire una stringa in lettere minuscole. Ciò può essere utile per rendere i dati uniformi o per facilitare la ricerca e la comparazione di stringhe.

## Come fare

Per convertire una stringa in lettere minuscole in Javascript, possiamo utilizzare il metodo integrato `toLowerCase()`. Basta chiamare questo metodo su una stringa e restituirà la stessa stringa in lettere minuscole.

```Javascript
let stringa = "CIAO A TUTTI!";
let convertita = stringa.toLowerCase();

console.log(convertita); // output: ciao a tutti!
```

Nota: è importante ricordare che il metodo `toLowerCase()` restituirà una nuova stringa in lettere minuscole, ma non modificherà la stringa originale.

Possiamo anche utilizzare il metodo `toLowerCase()` per controllare se due stringhe sono uguali, anche se sono scritte in diversi formati di lettere.

```Javascript
let stringa1 = "jAVaScRiPt";
let stringa2 = "JavaScript";

if (stringa1.toLowerCase() === stringa2.toLowerCase()) {
    console.log("Le due stringhe sono uguali!");
}
```

## Approfondimento

Il metodo `toLowerCase()` è basato sulle regole del linguaggio locale in cui viene eseguito il codice. Ciò significa che se stiamo lavorando in un ambiente dove la lingua madre è l'italiano, il metodo convertirà automaticamente le lettere accentate come "è" e "à". Tuttavia, se stiamo lavorando in un ambiente non italiano, le lettere accentate potrebbero non essere convertite correttamente. In questo caso, potremmo dover utilizzare altri metodi o libreria per ottenere il risultato desiderato.

## Vedi anche

- [Documentazione ufficiale di `toLowerCase()` in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Altri metodi utili per manipolare le stringhe in Javascript](https://www.freecodecamp.org/news/how-to-use-the-javascript-string-methods-in-your-code-d82c8b72f666/)