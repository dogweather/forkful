---
title:                "Analisi di html"
html_title:           "Clojure: Analisi di html"
simple_title:         "Analisi di html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con dati web, probabilmente dovrai affrontare il parsing di HTML. Il parsing di HTML è il processo di analisi del codice HTML per estrarre informazioni utili e strutturare i dati in modo da poterli utilizzare nel tuo programma.  

## Come fare

Per iniziare a parsare HTML con Clojure, hai bisogno di due librerie: `clj-http` e `enlive`. La prima ti consente di fare richieste HTTP alle pagine web, mentre la seconda ti aiuta a navigare e manipolare il codice HTML. Ecco un esempio di codice che fa una richiesta HTTP al sito di Wikipedia e estrae il contenuto di una tabella:

```Clojure
(require '[clj-http.client :as client])
(require '[net.cgrand.enlive-html :refer [html1]])

(def response (client/get "https://it.wikipedia.org/wiki/Italia"))
(def parsed-html (html1 (:body response)))

(def table (-> parsed-html
                (enlive/select [:table]))
    
(def rows (-> table
              (enlive/select [:tr])
              (remove #(= (enlive/at % [:.mw-headline]) "Inno")))) ; Rimuove la riga dell'inno nazionale

(def data (->> rows
             (map #(map enlive/text %)) ; Estrae il testo da ogni cella della tabella
             (remove #(= (count %) 0)) ; Rimuove le righe vuote
             (map #(nth % 1)))) ; Seleziona solo la seconda colonna di ogni riga

(println data)
; Output: ("Roma" "Italiano" "€" "+39" "EUR" "CEST" "WET" "IT" ".it")
```

## Approfondimento

Esistono diversi modi per parsare HTML in Clojure, ma `enlive` è uno dei più popolari e utilizzati. Oltre alla semplicità di utilizzo, offre anche un buon supporto per la manipolazione di dati HTML. Puoi utilizzare la sintassi di `enlive` per selezionare elementi nel codice HTML e navigare tra di essi utilizzando funzioni come `enlive/select` e `enlive/at`. Inoltre, puoi anche modificare direttamente il codice HTML utilizzando funzioni come `enlive/set-attr` e `enlive/append`. Questi sono solo alcuni esempi delle molteplici funzionalità offerte da `enlive`.

## Vedi anche

- Libreria `clj-http`: https://github.com/dakrone/clj-http
- Libreria `enlive`: https://github.com/cgrand/enlive
- Esempi di utilizzo di `enlive`: https://github.com/cgrand/enlive-tutorial