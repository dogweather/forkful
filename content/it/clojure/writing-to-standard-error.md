---
title:    "Clojure: Scrivere su errore standard"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere utile quando si vuole visualizzare messaggi di errore specifici durante l'esecuzione di un programma o quando si vuole monitorare il funzionamento di un'applicazione.

## Come fare

Per scrivere su standard error in Clojure, possiamo utilizzare la funzione `println` seguita dal simbolo `>` per indicare che il messaggio deve essere stampato su standard error. Ad esempio:

```Clojure
(println> "Questo messaggio sarà stampato su standard error")
```

Questo produrrà l'output: `Questo messaggio sarà stampato su standard error` su standard error.

## Approfondimento

Scrivere su standard error è utile quando si vuole distinguere tra messaggi di errore e messaggi di output. A differenza della funzione `println` che stampa su standard output, la funzione `println>` ci permette di scrivere su standard error in modo più esplicito.

Inoltre, possiamo utilizzare la funzione `eprint` per scrivere messaggi di errore specifici e la funzione `flush` per svuotare il buffer di standard error e assicurarci che i messaggi vengano immediatamente visualizzati.

## Guarda anche

- [Funzioni di output su Clojure](https://clojuredocs.org/clojure.core/println>)
- [Tutorial su standard error in Clojure](https://clojuredocs.org/clojure.core/eprint)