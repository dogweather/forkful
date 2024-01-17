---
title:                "Trova la lunghezza di una stringa"
html_title:           "Arduino: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Trovare la lunghezza di una stringa è un'operazione comune per i programmatori. Consiste nel determinare il numero di caratteri presenti all'interno di una stringa di testo. Ciò può essere utile per molteplici ragioni, ad esempio per gestire la limitazione di spazio di memoria o per elaborare e manipolare le stringhe in modo più efficiente.

## Come fare:
Per trovare la lunghezza di una stringa in Arduino, è possibile utilizzare la funzione `length()`. Basta passare come argomento la variabile contenente la stringa e la funzione restituirà il numero di caratteri che contiene. Di seguito è presente un esempio di codice:

```Arduino
String testo = "Ciao mondo!";
int lunghezza = testo.length();
```

Il valore della variabile `lunghezza` sarà 11, poiché la stringa contiene 11 caratteri, inclusi gli spazi.

## Approfondimento:
Trovare la lunghezza di una stringa è una delle operazioni di base nella programmazione. È una parte fondamentale della gestione delle stringhe e viene utilizzata in molte situazioni diverse. Ci sono anche altre funzioni che possono essere utilizzate per ottenere la lunghezza di una stringa, come ad esempio `strlen()` in linguaggio C. È importante notare che la lunghezza di una stringa può essere influenzata dalla codifica dei caratteri utilizzata.

## Vedi anche:
- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)
- [Tutorial su come manipolare le stringhe in Arduino](https://create.arduino.cc/projecthub/SURYATEJA/use-string-data-type-in-arduino-uno-54756b?f=1)
- [Spiegazione dettagliata su come funzionano le stringhe in Arduino](https://maker.pro/arduino/projects/the-power-of-strings-in-arduino)