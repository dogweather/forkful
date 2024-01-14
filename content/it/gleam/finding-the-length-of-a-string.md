---
title:                "Gleam: Trova la lunghezza di una stringa"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

La ricerca della lunghezza di una stringa può sembrare un compito banale, ma è un'attività fondamentale per molte operazioni di programmazione. Conoscere la lunghezza di una stringa è importante per la gestione dei dati, la validazione dei dati di input e la manipolazione delle stringhe stesse.

## Come fare

Per trovare la lunghezza di una stringa in Gleam, puoi utilizzare la funzione `String.length`. Di seguito un esempio di codice che mostra come utilizzare questa funzione:

```Gleam
let test_string = "Questo è un esempio di stringa."
let length = String.length(test_string)
```

L'output di questo codice sarà 30, poiché la stringa "Questo è un esempio di stringa" è composta da 30 caratteri. 

Inoltre, è possibile utilizzare un ciclo `for` per calcolare la lunghezza di una stringa, iterando su ogni carattere e incrementando una variabile di lunghezza fino alla fine della stringa. Ad esempio:

```Gleam
let mut length = 0
let test_string = "Questo è un esempio di stringa."

for c in test_string do
    length = length + 1

```

## Approfondimento

Ci sono alcune cose da tenere a mente quando si calcola la lunghezza di una stringa. Innanzitutto, in Gleam le stringhe sono immutabili, il che significa che non possono essere modificate una volta create. Quindi, ogni volta che si esegue un'operazione su una stringa, come la ricerca della lunghezza, viene creata una nuova stringa. Questo può essere un costo computazionale elevato, soprattutto se si lavora con stringhe di grandi dimensioni.

Inoltre, quando si manipolano stringhe in Gleam, è importante tenere conto della codifica dei caratteri. Alcune operazioni, come la ricerca della lunghezza, possono comportare risultati diversi a seconda della codifica utilizzata. Ad esempio, in una stringa Unicode ci possono essere caratteri che occupano più di un byte, quindi la funzione `String.length` potrebbe restituire un valore diverso a seconda della codifica utilizzata.

## Vedi anche

- [Documentazione ufficiale di Gleam sulla gestione delle stringhe](https://gleam.run/documentation/std_lib/strings/)
- [Risorse di apprendimento Gleam](https://gleam.run/learn/) - per imparare ulteriori concetti di programmazione con Gleam.
- [Esempi di codice Gleam](https://github.com/gleam-lang/gleam/tree/master/examples) - per ispirazione e pratica nella scrittura di codice Gleam.