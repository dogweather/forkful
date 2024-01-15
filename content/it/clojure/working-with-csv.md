---
title:                "Lavorare con i file csv"
html_title:           "Clojure: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore alle prime armi o un esperto in cerca di un linguaggio funzionale, Clojure è quello che fa per te. Con la sua sintassi semplice e una vasta libreria di funzioni, Clojure è perfetto per lavorare con file CSV.

## Come Fare

Per iniziare a lavorare con file CSV in Clojure, è necessario prima importare la libreria "clojure.data.csv". Possiamo farlo semplicemente digitando il seguente codice all'inizio del nostro file:

```Clojure
(require '[clojure.data.csv :as csv])
```
Ora possiamo leggere un file CSV usando la funzione "csv/read-csv". Ad esempio, se abbiamo un file chiamato "users.csv" con i campi "nome", "cognome" e "età", possiamo leggerlo e salvarlo in una variabile chiamata "utenti" in questo modo:

```Clojure
(utenti (csv/read-csv "users.csv"))
```
Possiamo quindi accedere agli elementi del file CSV utilizzando la sintassi standard Clojure di "primi-1", "primi-2", ecc. Ad esempio, per accedere al nome del primo utente nel file, possiamo scrivere:

```Clojure
(primi-1 utenti)
```
Che dovrebbe restituire il nome del primo utente nell'elenco.

## Approfondimento

Una volta ottenuta la lista dei dati dal file CSV, possiamo manipolarli utilizzando le funzioni Clojure standard. Ad esempio, possiamo utilizzare "filter" per filtrare gli utenti in base a determinate condizioni, "map" per applicare una funzione a ogni utente nella lista e "reduce" per combinare i dati in una singola struttura. Inoltre, Clojure offre alcune librerie esterne che semplificano ulteriormente la lettura, la scrittura e la manipolazione di file CSV, come ad esempio "clojure-csv" e "data.csv". Per saperne di più su come lavorare con CSV in Clojure, consulta la documentazione ufficiale e prova a scrivere alcuni esempi di codice.

## Vedi Anche

- [Documentazione ufficiale di Clojure](https://clojure.org/)
- [Libreria Clojure "clojure.data.csv"](https://cljdoc.org/d/clojure-data-csv/clojure.data.csv/0.1.4/doc/read-csv)
- [Esempi di codice per lavorare con CSV in Clojure](https://gist.github.com/russolsen/57a69ab50f2b3e9f701a)