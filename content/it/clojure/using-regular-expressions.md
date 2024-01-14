---
title:    "Clojure: Utilizzando le espressioni regolari"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono uno strumento potente per manipolare, controllare e analizzare i dati all'interno di un programma Clojure. Con l'aiuto delle espressioni regolari, è possibile trovare corrispondenze o modelli specifici all'interno di una stringa di testo e utilizzare queste informazioni per eseguire determinate operazioni o trasformazioni. Ciò rende le espressioni regolari utili per molti aspetti della programmazione, come la ricerca e la sostituzione di testo, la convalida di input utente e la formattazione di stringhe.

## Come fare

Per utilizzare le espressioni regolari in Clojure, è necessario importare il modulo regex inizialmente. Ci sono diverse funzioni nel modulo regex che possono essere utili per l'utilizzo delle espressioni regolari, come `re-matches`, `re-seq` e `re-find`. Ad esempio, possiamo utilizzare `re-matches` per trovare corrispondenze tra una stringa di input e un'espressione regolare fornita:

```Clojure
(require '[clojure.string :as str])
(require '[clojure.java.io :as io])
(use '[clojure.java.io :only (reader)])

(def input-string "123 456 789")
(def regex #"\d+") ;; corrisponde a qualsiasi numero nella stringa
(str/join "," (re-matches regex input-string)) ;; output: "123,456,789"
```

Questo è solo un semplice esempio, ma con un po' di pratica e comprensione delle espressioni regolari, è possibile effettuare molte altre operazioni utilizzando il modulo regex e le funzioni che offre.

## Approfondimenti

Le espressioni regolari sono basate su un linguaggio a sé stante, con una sintassi specifica per la creazione dei modelli di ricerca. Ci sono molti siti e risorse online che possono aiutare a imparare l'uso delle espressioni regolari, come regex101.com o regexone.com. Inoltre, c'è una vasta comunità di programmatori che utilizzano regolarmente le espressioni regolari, quindi non esitate a chiedere aiuto o cercare consigli su forum e community di programmazione.

## Vedi anche

- [Clojure Regex Reference Guide](https://clojure.org/reference/regular_expressions)
- [Regular Expressions Cookbook in Clojure](https://github.com/javagony/regex-cookbook-clojure)
- [Tutorial sulle espressioni regolari in Clojure](https://www.vividcortex.com/blog/2015/08/19/clojure-regex-tutorial)