---
title:    "Javascript: Estrarre sottostringhe"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Estrazione di sottoristringhe è un'operazione comune nel mondo della programmazione, specialmente in Javascript. Questo è utile per ottenere solo una parte di una stringa più lunga, che può essere poi utilizzata per vari scopi come la validazione dei dati o la manipolazione delle stringhe stesse.

## Come
Per estrarre una sottoristringa in Javascript, puoi utilizzare il metodo `substring()`. Questo metodo richiede due parametri: l'indice di partenza e l'indice finale della sottoristringa che si desidera ottenere. Vediamo un esempio pratico:

```Javascript
let parola = "Ciao, mondo!";
let sottostringa = parola.substring(0, 4);
console.log(sottostringa);

// Output: Ciao
```

In questo caso, abbiamo assegnato la stringa "Ciao, mondo!" alla variabile `parola`. Poi, abbiamo utilizzato il metodo `substring()` per estrarre i primi 4 caratteri, partendo dall'indice 0 della stringa. Il risultato è stato assegnato alla variabile `sottostringa` e visualizzato tramite `console.log()`.

Puoi anche utilizzare indici negativi con il metodo `substring()`, che significa che contano all'indietro dalla fine della stringa. Vediamo un esempio:

```Javascript
let parola = "Ciao, mondo!";
let sottostringa = parola.substring(-4, 6);
console.log(sottostringa);

// Output: mondo
```

In questo caso, abbiamo assegnato la stringa "Ciao, mondo!" alla variabile `parola` e utilizzato l'indice -4 come punto di partenza e 6 come punto finale. Ciò significa che iniziamo a contare dalla fine della stringa e andiamo indietro di 4 caratteri per ottenere "mondo".

## Approfondimento
Oltre al metodo `substring()`, esistono anche altri modi per estrarre sottoristringhe in Javascript. Ad esempio, è possibile utilizzare il metodo `slice()`, che funziona in modo simile a `substring()` ma può gestire anche indici negativi. Altri metodi utili sono `substr()`, che richiede come parametri l'indice di partenza e la lunghezza della sottoristringa, e `split()`, che suddivide la stringa in un array utilizzando un delimitatore specificato.

Inoltre, è importante tenere a mente che gli indici delle stringhe in Javascript iniziano sempre da 0 e l'ultimo carattere ha indice n-1, dove n è la lunghezza della stringa. Inoltre, è possibile utilizzare il metodo `length` per ottenere la lunghezza di una stringa.

## Vedi Anche
- [Documentazione di MDN su il metodo `substring()`](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Estrazione di sottoristringhe in Javascript: tutto quello che devi sapere](https://www.html.it/articoli/estrazione-sottoristringhe-javascript/)

Grazie per aver letto questo articolo su come estrarre sottoristringhe in Javascript. Speriamo che sia stato utile per comprendere meglio questa operazione comune nella programmazione. Ricorda di consultare questi link per ulteriori risorse e approfondimenti. Buona codifica!