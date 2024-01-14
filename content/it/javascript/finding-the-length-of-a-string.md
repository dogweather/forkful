---
title:                "Javascript: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive un codice Javascript, a volte si ha bisogno di ottenere la lunghezza di una stringa. Questo può essere utile per effettuare operazioni come controllare la validità dei dati inseriti dall'utente o manipolare una stringa in base alla sua lunghezza. Vediamo come è possibile trovare la lunghezza di una stringa utilizzando Javascript.

## Come fare

Per trovare la lunghezza di una stringa in Javascript, possiamo utilizzare il metodo `length`. Vediamo un esempio pratico:

```javascript
const stringa = "Ciao a tutti!";

console.log(`La lunghezza della stringa è: ${stringa.length}`);

// Output: La lunghezza della stringa è: 12
```

Come si può vedere, il metodo `length` restituisce il numero di caratteri presenti all'interno della stringa. In questo caso, la stringa "Ciao a tutti!" contiene 12 caratteri.

È importante notare che il metodo `length` funziona anche per stringhe vuote:

```javascript
const stringaVuota = "";

console.log(`La lunghezza della stringa vuota è: ${stringaVuota.length}`);

// Output: La lunghezza della stringa vuota è: 0
```

Inoltre, il metodo `length` può essere utilizzato anche per ottenere la lunghezza di una stringa di numeri:

```javascript
const numeri = "12345";

console.log(`La lunghezza della stringa di numeri è: ${numeri.length}`);

// Output: La lunghezza della stringa di numeri è: 5
```

## Approfondimento

In Javascript, le stringhe sono un tipo di dato primitivo e il loro valore è immutabile. Ciò significa che, una volta creata una stringa, non è possibile modificarne il contenuto. Tuttavia, è possibile accedere a un carattere specifico all'interno di una stringa utilizzando la loro posizione o indice.

Ad esempio, se volessimo ottenere il terzo carattere all'interno della stringa "Ciao a tutti!", possiamo farlo utilizzando la seguente sintassi: `stringa[2]`. Ricordiamo che gli indici iniziano sempre da 0, quindi il terzo carattere sarebbe il secondo indice.

```javascript
const stringa = "Ciao a tutti!";

console.log(`Il terzo carattere della stringa è: ${stringa[2]}`);

// Output: Il terzo carattere della stringa è: a
```

È possibile utilizzare questa stessa logica per accedere a qualsiasi carattere all'interno di una stringa utilizzando l'indice corrispondente.

## Vedi Anche

- [Documentazione di Mozilla su `length`](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Tutorial su come accedere ai caratteri di una stringa in Javascript](https://www.w3schools.com/js/js_string_access.asp)