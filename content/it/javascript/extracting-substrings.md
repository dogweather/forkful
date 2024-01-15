---
title:                "Estrazione di sottostringhe"
html_title:           "Javascript: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se si sta lavorando con stringhe di testo in Javascript, potrebbe essere necessario estrarre una parte di quella stringa. Ad esempio, se si ha il nome completo di una persona e si vuole mostrare solo il cognome o se si ha un URL e si vuole estrarre solo il percorso.

## Come Fare

Per estrarre una sottostringa da una stringa in Javascript, possiamo utilizzare il metodo `substring()`. Prende due parametri: il primo è l'indice di inizio della sottostringa e il secondo (opzionale) è l'indice di fine. Se viene omesso il secondo parametro, verrà automaticamente estratta la parte della stringa fino alla fine.

Esempio di codice:
```Javascript
let stringa = "Ciao mondo!";
let sottostringa = stringa.substring(5);
console.log(sottostringa);
```
Output:
```Javascript
"mondo!"
```

È anche possibile utilizzare il metodo `slice()`, che funziona allo stesso modo di `substring()` ma permette anche di indicare indici negativi (che iniziano dalla fine della stringa). Inoltre, `slice()` può essere utilizzato anche per dividere una stringa in più parti.

Esempio di codice:
```Javascript
let stringa = "Javascript è fantastico";
let sottostringa1 = stringa.slice(0, 9);
let sottostringa2 = stringa.slice(-11);
console.log(sottostringa1);
console.log(sottostringa2);
```
Output:
```Javascript
"Javascript"
"fantastico"
```

## Approfondimento

Il metodo `substr()` è simile a `substring()` ma invece di prendere come secondo parametro l'indice di fine, prende come secondo parametro la lunghezza della sottostringa. Inoltre, se viene passato un indice negativo come primo parametro, verrà contato partendo dalla fine della stringa.

Esempio di codice:
```Javascript
let stringa = "Bellissima giornata";
let sottostringa = stringa.substr(6, 9);
console.log(sottostringa);
```
Output:
```Javascript
"mi giorn"
```

Un concetto importante da comprendere è che in Javascript le stringhe sono immutabili, il che significa che non possono essere modificate direttamente. Quindi, quando si estrae una sottostringa da una stringa, viene in realtà creata una nuova stringa.

## Vedi anche

- Documentazione ufficiale di Javascript su stringhe: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String
- Articolo su come manipolare stringhe in Javascript: https://www.html.it/pag/49573/manipolare-le-stringhe-in-javascript/