---
title:                "Unione delle stringhe"
html_title:           "TypeScript: Unione delle stringhe"
simple_title:         "Unione delle stringhe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un'operazione comune nella programmazione, soprattutto quando si lavora con testi o dati ottenuti da diverse fonti. Unire più stringhe in una sola può semplificare il codice e rendere più leggibile il risultato finale.

## Come Fare

In TypeScript, è possibile concatenare stringhe utilizzando l'operatore "+" o il metodo "concat()". Ecco un esempio:

```TypeScript
let nome = "Mario";
let cognome = "Rossi";

let nomeCompleto = nome + " " + cognome;
// output: "Mario Rossi"

let nomeCompleto2 = nome.concat(" ", cognome);
// output: "Mario Rossi"
```

In entrambi i casi, una nuova stringa viene creata unendo le due variabili, separandole con uno spazio vuoto. È importante notare che l'operatore "+" può essere utilizzato anche per concatenare stringhe con altri tipi di dati, come numeri o variabili.

## Approfondimento

Esistono diverse tecniche per concatenare stringhe in modo più avanzato. Ad esempio, se si hanno più di due stringhe da unire, si può utilizzare il metodo "join()" che permette di specificare un separatore tra ogni elemento della lista. Ecco un esempio:

```TypeScript
let ingredienti = ["farina", "latte", "uova"];

let preparazione = ingredienti.join(", ");
// output: "farina, latte, uova"
```

È anche possibile utilizzare il metodo "slice()" per estrarre una parte di una stringa e concatenarla con un'altra. Ad esempio:

```TypeScript
let animali = "gatto, cane, coniglio";

let preferito = animali.slice(0, 5) + animali.slice(10, 14);
// output: "gatto, coniglio"
```

Infine, esistono anche librerie e moduli di terze parti che offrono funzioni più avanzate per la manipolazione di stringhe, come la libreria "lodash". È importante sempre controllare la documentazione ufficiale per scoprire le migliori pratiche e le funzionalità disponibili.

## Vedi Anche

- [Documentazione ufficiale di TypeScript sull'operatore "+" e il metodo "concat()"](https://www.typescriptlang.org/docs/handbook/strings.html#string-concatenation)
- [Esempi di utilizzo dei metodi "join()" e "slice()"](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
- [Libreria "lodash" per la manipolazione di stringhe](https://lodash.com/docs/4.17.11#_.truncate)