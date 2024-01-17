---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Clojure: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa & Perchè?

L'utilizzo delle espressioni regolari è un modo molto efficiente per cercare e manipolare testo in un programma Clojure. È uno strumento potente che permette ai programmatori di trovare corrispondenze precise o modelli all'interno di una stringa di testo. Ciò rende possibile l'elaborazione di grandi quantità di dati in modo rapido e preciso.

## Come si fa:

In Clojure, è possibile utilizzare espressioni regolari con la funzione `re-find` per trovare la prima corrispondenza in una stringa e `re-seq` per trovare tutte le corrispondenze. Ad esempio:

```Clojure
(re-find #"re.*" "espressioni regulari")
```
Produrrà l'output "regulari". 

Se invece si desidera trovare tutte le parole che iniziano con la lettera "t" in una lista, si può utilizzare:

```Clojure
(re-seq #"t\w+" ["tavolo" "triste" "tetto" "cane"])
```

Che produrrà l'output `("tavolo" "triste" "tetto")`

## Approfondimento:

Le espressioni regolari sono state introdotte per la prima volta negli anni '50 dal matematico Stephen Cole Kleene e sono diventate uno strumento fondamentale per l'analisi e la manipolazione del testo nei linguaggi di programmazione. In Clojure, sono supportate dalla libreria Java `java.util.regex` e sono anche disponibili altre librerie di terze parti come `clojure.string` e `re-find`.

Se preferisci utilizzare un approccio diverso alle espressioni regolari, puoi utilizzare anche la funzione `match` della libreria Clojure core. Utilizzando questi diversi strumenti, puoi scegliere quello che meglio si adatta alle tue esigenze.

## Vedi anche:

- [ClojureDocs - Regular Expressions](https://clojuredocs.org/clojure.regex) 
- [Official Java Documentation - Regular Expressions](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Clojure for the Brave and True - Regular Expressions](https://www.braveclojure.com/regular-expressions/)