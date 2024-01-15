---
title:                "Convertire una stringa in minuscolo"
html_title:           "Javascript: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Converting a string to lower case è necessario quando si vuole manipolare le stringhe in modo uniforme, ad esempio per facilitare la ricerca o il confronto tra di esse.

## Come fare
Per convertire una stringa in lower case in Javascript, possiamo utilizzare il metodo `toLowerCase()`. Questo metodo restituisce una nuova stringa con tutti i caratteri della stringa originale trasformati in caratteri minuscoli.

```javascript
let stringa = "QuestA è Una StRinGA dI eSempio";
let stringaMinuscola = stringa.toLowerCase();
console.log(stringaMinuscola); // output: "questa è una stringa di esempio"
```

Il metodo `toLowerCase()` non modifica la stringa originale, ma ne restituisce una nuova. È possibile assegnare il risultato a una nuova variabile o sovrascrivere quella originale.

Se abbiamo a che fare con un array di stringhe, possiamo utilizzare il metodo `forEach()` per applicare la conversione a tutte le stringhe contenute nell'array.

```javascript
let array = ["Stringa1", "StriNGA2", "STRINGA3"];
array.forEach((stringa, index) => {
  array[index] = stringa.toLowerCase();
});
console.log(array); // output: ["stringa1", "stringa2", "stringa3"]
```

## Approfondimento
Il metodo `toLowerCase()` è particolarmente utile quando si tratta di manipolare stringhe alfanumeriche, poiché i caratteri maiuscoli e minuscoli verranno considerati uguali in termini di ricerca e confronto.

Inoltre, questo metodo tiene conto delle diverse convenzioni di scrittura delle parole, come ad esempio il caso del titolo o il caso del camelCase (in cui le parole sono concatenate senza spazi e la prima lettera di ogni parola tranne la prima è in maiuscolo).

Ad esempio, se vogliamo cercare una determinata parola in una stringa, utilizzando `toLowerCase()` possiamo assicurarci che non ci siano problemi di caratteri maiuscoli o minuscoli:

```javascript
let stringa = "Questo è un Esempio di stringa.";
let parola = "esempio";
if (stringa.toLowerCase().includes(parola.toLowerCase())) {
  console.log("La parola è presente nella stringa.");
} else {
  console.log("La parola non è presente nella stringa.");
}
// output: "La parola è presente nella stringa."
```

Inoltre, è importante tenere presente che `toLowerCase()` non funziona solo con le lettere dell'alfabeto latino, ma anche con gli alfabeti di altre lingue.

## Vedi anche
- [MDN Web Docs: String.prototype.toLowerCase()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [W3Schools: JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [Tutorialspoint: JavaScript String toLowerCase() Method](https://www.tutorialspoint.com/javascript/javascript_string_tolowercase.htm)