---
title:                "Scrivere su standard error"
html_title:           "Clojure: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere su standard error significa inviare un messaggio di errore o avviso che non fa parte dell'output normale del programma. I programmatori lo fanno per aiutare a identificare problemi e migliorare la gestione degli errori nel loro codice.

## Come fare:

```Clojure
(defn scrivi-su-std-err [testo]
  (println (with-out-str
             (with-out-str
               (let [*err* *out*]
                 (println testo))))))

(scrivi-su-std-err "Questo è un messaggio di errore!") ; output: Questo è un messaggio di errore!
```

## Approfondimento:

Scrivere su standard error è stato introdotto inizialmente nei sistemi Unix come una parte del sistema di gestione degli errori. In alcuni casi, utilizzare questo approccio può essere preferibile rispetto a scrivere un messaggio su standard output, poiché si separa chiaramente il contenuto normale del programma dai messaggi di errore. Altre opzioni includono l'utilizzo di log file o librerie apposite per la gestione degli errori.

## Vedi anche:
