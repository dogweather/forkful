---
title:    "Clojure: Stampa dell'output di debug"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché
Scrivere output di debug può essere utile quando si cerca di individuare e risolvere errori nel proprio codice. Può fornire informazioni utili sulla logica del programma e aiutare a identificare eventuali problemi.

## Come fare
Per stampare l'output di debug in Clojure, è possibile utilizzare la funzione `println` seguita da ciò che si desidera visualizzare. Ad esempio:

```Clojure
(println "Il mio messaggio di debug")
```

Questo stamperà "Il mio messaggio di debug" nella console. Inoltre, è possibile utilizzare la funzione `prn` per stampare in modo più formattato e la funzione `str` per convertire variabili di diversi tipi in stringhe. Ad esempio:

```Clojure
(def nome "Laura")
(def età 30)
(prn "Ciao, mi chiamo" nome "e ho" età "anni")
```

Questo stamperà "Ciao, mi chiamo Laura e ho 30 anni" nella console.

## Approfondimento
Una delle funzionalità più utili per il debug in Clojure è l'utilizzo della macro `dbg` del pacchetto `clojure.tools.logging`. Questa macro permette di stampare non solo il messaggio di debug, ma anche la funzione e il numero di riga da cui è stata chiamata. Ad esempio:

```Clojure
(require '[clojure.tools.logging :as log])
(def nome "Anna")
(def età 25)
(log/dbg "Ciao, mi chiamo" nome "e ho" età "anni")
```

Questo stamperà "Il mio messaggio di debug" seguito da "Nome della funzione: numero riga", ad esempio "Ciao, mi chiamo Anna e ho 25 anni - user:27" nella console. Questo può essere molto utile per individuare errori e comprendere meglio il flusso del programma.

## Vedi anche
- [Documentazione ufficiale di Clojure](https://clojure.org/)
- [Guida al debug in Clojure](http://www.braveclojure.com/debugging/)
- [Clojure Debug Repl](https://github.com/Day8/re-frame/blob/master/docs/DEBUGGING.md)