---
title:                "Clojure: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore e lavori con testo, a volte può capitare di dover sostituire una determinata parola o frase all'interno di un file. Con Clojure, è possibile automatizzare questo processo e risparmiare tempo, rendendo il tuo lavoro più efficiente.

## Come Fare

Per iniziare, dovrai importare la libreria `clojure.string` nel tuo progetto Clojure. Questa libreria fornisce diverse funzioni utili per la manipolazione delle stringhe, tra cui la funzione `replace`, che ci permetterà di sostituire il testo all'interno di una stringa.

```Clojure
(require '[clojure.string :as str])
```

Ora che abbiamo importato la libreria, possiamo utilizzare la funzione `replace` passandogli tre argomenti: la stringa in cui vogliamo sostituire il testo, la parola o frase che vogliamo sostituire e il testo di sostituzione.

Ad esempio, se vogliamo sostituire la parola "cane" con "gatto" all'interno di una stringa, possiamo scrivere il seguente codice:

```Clojure
(str/replace "Il cane è il mio animale preferito" "cane" "gatto")
```

L'output sarà:

```Clojure
"Il gatto è il mio animale preferito"
```

La funzione `replace` sostituirà tutte le occorrenze della parola "cane" con "gatto" all'interno della stringa.

## Approfondimento

La funzione `replace` accetta in realtà anche un quarto argomento opzionale, rappresentato da un numero che indica il numero massimo di sostituzioni da effettuare. In questo modo, possiamo decidere di sostituire solo la prima o le prime due occorrenze di una parola specifica.

Inoltre, la libreria `clojure.string` offre anche altre funzioni utili per la manipolazione delle stringhe, come ad esempio `split` per separare una stringa in un vettore di stringhe in base a un determinato separatore, o `join` per unire una lista di stringhe in una singola stringa.

## Vedi Anche

- Documentazione della libreria `clojure.string`: https://clojuredocs.org/clojure.string
- Tutorial su come utilizzare la libreria `clojure.string`: https://purelyfunctional.tv/guide/clojure-string-cheatsheet/ 
- Esempi pratici di utilizzo della libreria `clojure.string`: https://gist.github.com/acheckler/7931540