---
title:                "Javascript: Convertire una stringa in minuscolo"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si lavora con stringhe di testo in Javascript, può essere necessario convertire una stringa in minuscolo. Ciò può essere utile per confrontare le stringhe in modo efficiente, eliminare le differenze di maiuscole e minuscole, o semplicemente avere una stringa in un formato standard. In questo articolo, esploreremo il processo di conversione di una stringa in minuscolo utilizzando il linguaggio di programmazione Javascript.

## Come fare

Per convertire una stringa in minuscolo in Javascript, possiamo utilizzare il metodo `toLowerCase()`. Questo metodo prende una stringa come input e restituisce una nuova stringa in minuscolo. Vediamo un esempio di codice:

```Javascript
let testo = "Questo è un TESTO di Prova";
let testoMinuscolo = testo.toLowerCase();
console.log(testoMinuscolo);
```
Output: "questo è un testo di prova"

Il metodo `toLowerCase()` non modifica la stringa originale, ma restituisce una nuova stringa in minuscolo. Inoltre, tiene conto delle lettere accentate e speciali, come nel primo esempio in cui la "E" accentata viene convertita in "e" minuscola.

Possiamo anche utilizzare questa tecnica con variabili già definite, come nel seguente esempio:

```Javascript
let nome = "Mario";
let cognome = "Rossi";
let nomeCompleto = nome.toLowerCase() + " " + cognome.toLowerCase();
console.log(nomeCompleto);
```
Output: "mario rossi"

È importante notare che il metodo `toLowerCase()` è sensibile alla lingua utilizzata nel nostro codice. Ad esempio, se si utilizza una lingua diversa dall'italiano, potrebbe esserci una differenza nella conversione delle lettere accentate.

## Approfondimento

Oltre al metodo `toLowerCase()`, esistono altri modi per convertire una stringa in minuscolo in Javascript. Ad esempio, possiamo utilizzare il metodo `toLocaleLowerCase()` per tenere conto delle differenze di lingua e caratteri speciali. Inoltre, se dobbiamo gestire la conversione di più stringhe, possiamo creare una funzione personalizzata che combina diversi metodi per ottenere il risultato desiderato.

Inoltre, è importante tenere conto del contesto in cui viene utilizzata la conversione di una stringa in minuscolo. Potrebbe essere necessario gestire eventuali spazi vuoti o formattare la stringa in modo specifico prima di eseguire la conversione.

## Vedi anche

- [Metodo toLowerCase() su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Convertire una stringa in minuscolo con Javascript](https://www.html.it/guide/introduzione-a-javascript/convertire-una-stringa-in-minuscolo-con-javascript/)
- [Funzione personalizzata per la conversione di una stringa in minuscolo](https://www.codegrepper.com/code-examples/javascript/javascript+convert+string+to+lowercase+function)