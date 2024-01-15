---
title:                "Maiuscolare una stringa"
html_title:           "Javascript: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa può sembrare un'operazione semplice, ma può essere molto utile per migliorare la leggibilità del testo o per soddisfare requisiti specifici di un programma. Inoltre, imparare a capitalizzare una stringa è un'abilità fondamentale per ogni programmatore Javascript.

## Come Fare

Per capitalizzare una stringa in Javascript, possiamo utilizzare il metodo `toUpperCase()` o `charAt()` insieme ad altri metodi come `slice()` o `substring()`:

```
// Utilizzando toUpperCase()
let stringa = "ciao mondo";
console.log(stringa.toUpperCase()); //output: CIAO MONDO

// Utilizzando charAt()
let lettera = stringa.charAt(0).toUpperCase() + stringa.slice(1);
console.log(lettera); //output: Ciao mondo
```

Possiamo anche creare una funzione che controlli ogni carattere della stringa e trasformi in maiuscolo solo la prima lettera di ogni parola:

```
function capitalizeString(str) {
  let parole = str.split(" ");
  for (let i = 0; i < parole.length; i++) {
    parole[i] = parole[i].charAt(0).toUpperCase() + parole[i].substring(1);
  }
  return parole.join(" ");
}

console.log(capitalizeString("ciao mondo")); //output: Ciao Mondo
```

Utilizzando questi metodi, possiamo capitalizzare una stringa in modo efficiente e personalizzato, a seconda delle nostre esigenze.

## Approfondimento

Ci sono alcuni dettagli importanti da considerare quando si capitalizza una stringa in Javascript. Uno di questi è il fatto che i metodi `toUpperCase()` e `charAt()` non modificano direttamente la stringa originale, ma restituiscono una nuova stringa capitalizzata. Invece, la funzione `capitalizeString()` modificava direttamente la stringa originale.

Inoltre, questi metodi sono sensibili alle maiuscole e minuscole, quindi se la stringa contiene già lettere maiuscole, queste verranno lasciate intatte.

Un'altra cosa da tenere in considerazione è che i metodi `charAt()` e `substring()` contano gli indici dei caratteri a partire da zero, mentre il primo carattere di una stringa ha indice 1. Questo può causare confusione se non si è abituati a questo comportamento.

Infine, ci sono anche metodi di terze parti che possono essere utilizzati per capitalizzare una stringa, come ad esempio la libreria "change-case", che offre una varietà di opzioni per capitalizzare una stringa in modo più specifico.

## Vedi Anche

- [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Change Case library](https://github.com/blakeembrey/change-case)