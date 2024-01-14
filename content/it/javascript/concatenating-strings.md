---
title:                "Javascript: Concatenazione di stringhe"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione delle stringhe è un concetto fondamentale nella programmazione JavaScript che consente di unire più stringhe in una sola. Questo può essere utile per creare una stringa personalizzata con dati dinamici o per formattare l'output di un programma.

## Come fare

Per concatenare le stringhe in JavaScript, si utilizza l'operatore "+" oppure il metodo "concat()". Vediamo un esempio di entrambi i metodi:

```
// Utilizzo dell'operatore "+"

let nome = "Marco";
let cognome = "Rossi";
let nomeCompleto = nome + " " + cognome;
console.log(nomeCompleto); // Output: Marco Rossi
```

```
// Utilizzo del metodo "concat()"

let frase1 = "Ciao";
let frase2 = "a tutti";
let saluto = frase1.concat(" ", frase2);
console.log(saluto); // Output: Ciao a tutti
```

È possibile concatenare più di due stringhe e anche variabili possono essere usate all'interno dell'operazione di concatenazione.

```
let saluto = "Ciao";
let nome = "Matteo";
let messaggio = saluto + " " + nome + ", benvenuto!";
console.log(messaggio); // Output: Ciao Matteo, benvenuto!
```

## Approfondimento

In JavaScript, le stringhe sono immutabili, il che significa che non possono essere modificate. Quindi, ogni volta che si esegue un'operazione di concatenazione, viene creata una nuova stringa anziché modificare la stringa originale. Questo è importante da tenere presente quando si lavora con grandi quantità di dati, in quanto l'utilizzo eccessivo della concatenazione può avere un impatto negativo sulle prestazioni del programma.

Esistono anche altri metodi utili per manipolare le stringhe in JavaScript, come ad esempio "trim()" per eliminare gli spazi vuoti all'inizio e alla fine di una stringa, "toUpperCase()" per rendere tutte le lettere maiuscole e "split()" per dividere una stringa in un array di sotto-stringhe basate su un separatore specificato.

## Vedi anche

- Documentazione su concatenazione di stringhe in JavaScript: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/concat
- Metodi stringa in JavaScript: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String