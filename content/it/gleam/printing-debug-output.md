---
title:                "Stampare output di debug"
html_title:           "Gleam: Stampare output di debug"
simple_title:         "Stampare output di debug"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Printare l'output di debug è una pratica comune tra i programmatori che consiste nel visualizzare informazioni aggiuntive durante l'esecuzione di un programma. Ciò può aiutare a identificare errori e problemi nel codice, semplificando la fase di debug.

## Come fare:
Per aggiungere output di debug nel tuo codice Gleam, usa la funzione `debug.print`. Puoi stampare qualsiasi tipo di dato, incluso record e tuple.

Esempio:
```
Gleam...
let mia_variabile = "Ciao!"
mia_variabile |> debug.print
...
``` 

Output:
```
"Ciao!"
```

Puoi anche usare `debug.print` con più argomenti per stampare più di un valore:
```
Gleam...
let mio_numero = 42
let mio_nome = "Alice"
debug.print(mio_numero, mio_nome)
...
```

Output:
```
42
"Alice"
```

## Approfondimento:
Aggiungere output di debug è una pratica comune nella programmazione, ma assicurati di rimuoverlo una volta che hai risolto i problemi, altrimenti potrebbe avere un impatto negativo sulle prestazioni del tuo programma.

Un'alternativa alla stampa di output di debug è l'uso di un debugger, che consente di rallentare l'esecuzione del programma e di esaminare il valore delle variabili in ogni istante. Tuttavia, l'aggiunta di output di debug può essere più rapida e semplice per risolvere piccoli problemi.

L'implementazione di `debug.print` in Gleam utilizza il modulo `logger`, che offre una varietà di funzionalità avanzate per la registrazione di messaggi di debug.

## Vedi anche:
- [Documentazione ufficiale di Gleam](https://gleam.run/documentation/)
- [Articolo su come risolvere i problemi di debugging in modo efficace](https://blog.bugsnag.com/how-to-debug/)
- [Modulo logger di Gleam](https://github.com/gleam-lang/logger)