---
title:    "Javascript: Rendere maiuscola una stringa"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché
Capitare una stringa può essere utile quando si vuole dare enfasi o risalto ad una determinata parola o frase all'interno di un testo. Inoltre, è una buona pratica di codifica per garantire la coerenza nei dati di input.

## Come
Per capitalizzare una stringa in Javascript, possiamo utilizzare il metodo `.toUpperCase()` che converte tutti i caratteri della stringa in lettere maiuscole. Possiamo anche utilizzare il modulo `toLocaleUpperCase()` per gestire eventuali caratteri speciali o accenti.

```Javascript
var str = "ciao a tutti!";

console.log(str.toUpperCase());
// Output: CIAO A TUTTI!

console.log(str.toLocaleUpperCase());
// Output: CIAO A TUTTI!
```

## Approfondimento
È importante notare che questi metodi non modificano direttamente la stringa originale, ma ne restituiscono una nuova versione con i caratteri capitalizzati. Per modificare la stringa originale, possiamo assegnare la nuova stringa a una variabile o utilizzare il metodo `.replace()` per sostituire la stessa stringa con la versione capitalizzata.

```Javascript
// Utilizzando una variabile:
var str = "Ciao a tutti!";
var newStr = str.toUpperCase();

console.log(str); // Output: Ciao a tutti!
console.log(newStr); // Output: CIAO A TUTTI!

// Utilizzando il metodo .replace():
var str = "Ciao a tutti!";
str = str.replace(str, str.toUpperCase());

console.log(str); // Output: CIAO A TUTTI!
```

## Vedi anche
- [Metodo toUpperCase() su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Metodo replace() su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Modulo I18n su Javascript](https://www.i18next.com/)