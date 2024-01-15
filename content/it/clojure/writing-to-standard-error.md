---
title:                "Scrittura su errore standard"
html_title:           "Clojure: Scrittura su errore standard"
simple_title:         "Scrittura su errore standard"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un'operazione comune in Clojure per gestire errori e debugging. Ciò consente agli sviluppatori di identificare rapidamente eventuali problemi nel codice e di fornire informazioni dettagliate sulle cause degli errori.

## Come fare

Per scrivere su standard error in Clojure, usiamo la funzione `clojure.pprint/write-err` e passiamo ad essa il messaggio da stampare. Il suo output verrà quindi visualizzato sulla console di errore. Di seguito un esempio di codice:

```Clojure
(require '[clojure.pprint :refer [write-err]])
(write-err "Errore: valore non valido")
```

Il risultato dell'esempio sarà:

`Errore: valore non valido`

## Approfondimento

Scrivere su standard error è molto utile per la gestione degli errori, ma è anche importante conoscere le differenze tra standard out e standard error. In Clojure, standard out è utilizzato per la stampa di output di natura generale, mentre standard error è riservato per la gestione degli errori. Inoltre, standard error ha una priorità maggiore e verrà sempre stampato prima di standard out.

## Vedi anche
- Documentazione ufficiale di Clojure su `clojure.pprint/write-err`[https://clojuredocs.org/clojure.pprint/write-err]
- Tutorial su error handling in Clojure[https://www.braveclojure.com/functional-error-handling/]