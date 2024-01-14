---
title:    "Javascript: Estrazione di sottostringhe"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe da una stringa è una delle funzioni fondamentali della programmazione in Javascript. Se stai lavorando con grandi quantità di dati o necessiti di manipolare una stringa per ottenere solo una parte di essa, la capacità di estrarre una sottostringa può semplificare notevolmente il tuo codice. In breve, estrarre sottostringhe è essenziale per la gestione efficiente delle stringhe in Javascript.

## Come fare

Estrarre una sottostringa da una stringa in Javascript è semplice e può essere fatto in pochi passi. Innanzitutto, hai bisogno di una stringa in cui estrarre la sottostringa. Ad esempio, vogliamo estrarre la parola "casual" dalla seguente stringa:

```Javascript
var stringa = "Questo è un blog post casuale sulle sottolineature in Javascript.";
```

Ora che abbiamo la nostra stringa di riferimento, possiamo utilizzare il metodo `substring()` per estrarre la sottostringa desiderata. Il metodo `substring()` accetta due parametri: l'indice iniziale e l'indice finale della stringa da estrarre. Ad esempio, per estrarre la parola "casuale" possiamo utilizzare i seguenti parametri:

```Javascript
stringa.substring(26, 33);
```

Il primo parametro, 26, indica l'indice iniziale della parola "casuale" nella nostra stringa. Il secondo parametro, 33, indica l'indice finale, considerando che omessi omette tutte le lettere dalla posizione indicata fino alla fine della stringa. Quindi, il metodo `substring()` restituirà la sottostringa "casuale".

Un altro modo per estrarre sottostringhe in Javascript è utilizzare il metodo `slice()`. Questo metodo funziona in modo simile a `substring()`, ma accetta solo un parametro di indice iniziale. Di seguito è riportato un esempio di come utilizzare il metodo `slice()` per estrarre la stessa sottostringa "casuale":

```Javascript
stringa.slice(26);
```

Questo restituisce la stessa sottostringa, in quanto omessi omette tutte le lettere dalla posizione indicata fino alla fine della stringa. Ecco perché il parametro finale non è necessario per estrarre la sottostringa desiderata.

## Approfondimento

Oltre al metodo `substring()` e `slice()`, esistono anche altri metodi che possono essere utilizzati per estrarre sottostringhe da una stringa in Javascript. Ad esempio, il metodo `substr()` accetta due parametri come `substring()`, ma il secondo parametro rappresenta la lunghezza della sottostringa da estrarre anziché l'indice finale. Inoltre, è possibile utilizzare le proprieta `length` di una stringa per ottenere la lunghezza della stringa e utilizzarla come parametro.

Inoltre, è importante notare che il conteggio degli indici nelle stringhe inizia da 0. Ciò significa che la prima lettera di una stringa ha indice 0, la seconda lettera ha indice 1 e così via. Questo è un concetto importante da tenere a mente quando si lavora con l'estrazione delle sottostringhe in Javascript.

Inoltre, i metodi per estrarre sottostringhe possono essere combinati con altri metodi di manipolazione delle stringhe in Javascript, come `toUpperCase()` e `toLowerCase()`, per ottenere il risultato desiderato.

## Vedi anche

- [Documentazione MDN su substring()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Documentazione MDN su slice()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Documentazione MDN su substr()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [Documentazione MDN sulle stringhe in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String)