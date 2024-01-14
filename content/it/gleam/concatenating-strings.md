---
title:                "Gleam: Unione di stringhe"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare le stringhe è un'operazione fondamentale nella programmazione. Ci permette di unire più stringhe per creare una nuova stringa. Questo è utile quando si vuole creare un messaggio personalizzato che includa informazioni dinamiche o quando si vuole unire vari pezzi di testo.

## Come fare

Per concatenare le stringhe in Gleam, è possibile utilizzare il simbolo "+" o la funzione `"~"string.concat()`. Vediamo un esempio:

```Gleam
let nome = "Marco"
let saluto = "Ciao"

let messaggio = saluto + " " + nome
// oppure
let messaggio2 = string.concat(saluto, " ", nome)

debug.(messaggio) // output: Ciao Marco
debug.(messaggio2) // output: Ciao Marco
```

Come si può vedere, entrambi i metodi producono lo stesso risultato. È importante notare che quando si utilizza il simbolo "+", è necessario assicurarsi di aggiungere uno spazio bianco tra le stringhe in modo da ottenere una corretta formattazione del testo.

Le stringhe in Gleam sono immutabili, quindi è necessario creare una nuova stringa per aggiungere del testo. Tuttavia, possiamo combinare più stringhe in una volta utilizzando la funzione `~"string.concat()`:

```Gleam
let list_personalizza = ["Benvenuto", "nel", "nostro", "blog"]

let messaggio_personalizzato = string.concat(...list_personalizza)
debug.(messaggio_personalizzato) // output: Benvenuto nel nostro blog
```

## Approfondimenti

Nel nostro esempio precedente, abbiamo utilizzato l'operatore spread `...` per passare tutti gli elementi della lista come argomenti alla funzione `~"string.concat()`. Inoltre, è possibile concatenare non solo le stringhe, ma anche variabili di altri tipi di dati, come numeri, booleani, ecc.

Inoltre, è importante tenere a mente che l'operazione di concatenazione può essere costosa da un punto di vista delle prestazioni, specialmente se si lavora con un grande numero di stringhe. In questi casi, è consigliato utilizzare il tipo di dati `io.Appendable` per ottenere prestazioni migliori.

## Vedi anche

- Documentazione ufficiale di Gleam sulla manipolazione delle stringhe: https://gleam.run/documentation/core/string.html
- Una guida completa su come utilizzare i tipi di dati in Gleam: https://dev.to/gleam/gleam-101-types-18km