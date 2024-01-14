---
title:    "Gleam: Estrazione di sottostringhe"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perchè

Estrarre sottostringhe è una pratica comune nella programmazione, specialmente quando si lavora con stringhe di testo. Se sei nuovo alla programmazione o vuoi ampliare le tue conoscenze su questo argomento, continua a leggere per scoprire come estrarre sottostringhe utilizzando il linguaggio di programmazione Gleam.

## Come fare

Per estrarre una sottostringa in Gleam, puoi utilizzare la funzione `String.substr(start, length)` dove `start` è l'indice iniziale della sottostringa e `length` è la lunghezza della sottostringa desiderata. Ad esempio, se vogliamo estrarre una sottostringa da "Hello World!" che contiene solo la parola "World", possiamo utilizzare il seguente codice:

```Gleam
let original_string = "Hello World!"
let extracted_string = String.substr(6, 5)
```

Il valore di `extracted_string` sarà "World". È importante notare che gli indici delle stringhe in Gleam iniziano da 0, quindi il primo carattere ha indice 0.

Puoi anche estrarre una parte di una stringa utilizzando una sintassi simile a quella delle liste. Ad esempio, se vogliamo estrarre una sottostringa che contiene solo "Hello" da "Hello World!", possiamo utilizzare il seguente codice:

```Gleam
let original_string = "Hello World!"
let extracted_string = String.substr(0, 5)
```

Il valore di `extracted_string` sarà "Hello".

## Approfondimento

Oltre alla funzione `String.substr`, Gleam offre anche altre funzioni per estrarre sottostringhe in modo più specifico. Ad esempio, `String.left(number)` restituisce i primi `number` caratteri di una stringa, mentre `String.right(number)` restituisce gli ultimi `number` caratteri. Inoltre, la funzione `String.slice(start, end)` consente di estrarre una sottostringa specificando l'indice di inizio e l'indice finale della stringa desiderata.

Inoltre, è possibile utilizzare l'operatore di concatenazione `++` per combinare due o più stringhe insieme e quindi estrarre una sottostringa dal risultato finale.

## Vedi anche

- La documentazione ufficiale di Gleam sulla manipolazione delle stringhe: https://gleam.run/documentation/libraries/string.html
- Un tutorial su come utilizzare le stringhe in Gleam: https://youtu.be/IlOw1y8WxSg