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

## Cosa & Perché?

L'estrazione di sottostringhe (o "substrings") è una pratica comune tra i programmatori, che consiste nel prendere una porzione di una stringa più grande per lavorarci sopra in modo indipendente. Questo è particolarmente utile quando si ha bisogno di manipolare o analizzare solo parte di una stringa.

## Come fare:

Ecco un esempio semplice di come estrarre una sottostringa utilizzando il metodo `substring()` in Javascript:

```Javascript
let stringa = "Oggi è una bella giornata!"
let sottostringa = stringa.substring(5,10)
console.log(sottostringa)
```

Output: `è una`

In questo esempio, abbiamo utilizzato il metodo `substring()` su una variabile contenente una stringa e specificato gli indici dalla quale estrarre la sottostringa desiderata. Il primo numero corrisponde all'indice del primo carattere, mentre il secondo numero corrisponde all'indice del carattere successivo al nostro punto finale.

## Approfondimento:

L'estrazione di sottostringhe è stata una funzionalità implementata sin dai primi linguaggi di programmazione, come il Basic, per permettere ai programmatori di manipolare e analizzare testo più facilmente. In Javascript, oltre al metodo `substring()`, esistono anche altre opzioni come `slice()`, `substr()` e l'utilizzo dei bracket `[]` per estrarre singoli caratteri. Ognuna di queste alternative ha i suoi punti di forza e potrebbe essere più adatta in base alle esigenze specifiche del progetto.

## Vedi anche:

- La documentazione ufficiale di substring() su [MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- Una spiegazione dettagliata sull'estrazione di sottostringhe su [W3Schools](https://www.w3schools.com/jsref/jsref_substr.asp)
- Un confronto tra i vari metodi di estrazione di sottostringhe su [GeeksforGeeks](https://www.geeksforgeeks.org/difference-between-substr-vs-slice-vs-substring-in-javascript/)