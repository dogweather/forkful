---
title:                "Ricerca e sostituzione di testo"
html_title:           "Arduino: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La sostituzione di testo è una tecnica utilizzata dai programmatori per trovare e sostituire parti specifiche di codice all'interno di un programma. Questo può essere utile quando si vogliono apportare modifiche a più parti, anziché farlo una per una.

## Come fare:

Ecco un esempio di codice che utilizza la funzione `replace()` per sostituire la parola "ciao" con "salve":

```
Arduino.replace("ciao", "salve");
```

Questo codice sostituirà tutte le occorrenze di "ciao" con "salve" all'interno del programma.

## Approfondimento:

La tecnica di ricerca e sostituzione è stata introdotta nei primi anni della programmazione informatica, e da allora è stata utilizzata in molti linguaggi di programmazione. Tuttavia, ci sono anche altre tecniche alternative, come l'utilizzo delle espressioni regolari.

È importante prestare attenzione alla sintassi quando si utilizza la funzione `replace()` in Arduino. Invece di usare le virgolette aperte e chiuse `"` per indicare il testo da cercare e sostituire, bisogna utilizzare le virgolette singole `'`. Inoltre, è importante ricordare che la funzione ha due parametri: il testo da cercare e il testo con cui sostituirlo.

## Vedi anche:

Per maggiori informazioni sulla funzione `replace()` e su altre funzioni utili in Arduino, puoi consultare la documentazione ufficiale di Arduino: [https://www.arduino.cc/reference/en/functions/strings/replace/](https://www.arduino.cc/reference/en/functions/strings/replace/)

Puoi anche approfondire il concetto di espressioni regolari e il loro utilizzo nella sostituzione di testo in Arduino su Stack Overflow: [https://stackoverflow.com/questions/40277507/how-to-replace-a-word-with-another-word-using-regex-in-arduino/40278086](https://stackoverflow.com/questions/40277507/how-to-replace-a-word-with-another-word-using-regex-in-arduino/40278086)