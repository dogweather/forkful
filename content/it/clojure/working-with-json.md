---
title:                "Clojure: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si lavora con applicazioni web o API, ci si ritrova a dover gestire dati in formato JSON. Il JSON è un formato di scambio dati molto comune e versatile, quindi imparare a lavorare con esso può essere estremamente utile per i programmatori Clojure.

## Come fare

Per lavorare con JSON in Clojure, dobbiamo prima importare la libreria `clojure.data.json`. Utilizziamo la funzione `json/read-str` per leggere il nostro file JSON in una mappa Clojure:

```
(require '[clojure.data.json :as json])

(def json-data (json/read-str "{\"name\":\"John\",\"age\":30}"))
```

Possiamo quindi accedere ai valori della mappa utilizzando le normali funzioni di accesso Clojure come `get` e `get-in`:

```
(get json-data "name") ;; restituisce "John"
(get-in json-data ["age"]) ;; restituisce 30
```

Per convertire una mappa Clojure in JSON, utilizziamo la funzione `json/write-str`:

```
(def person {:name "Jane" :age 25})

(json/write-str person) ;; restituisce "{\"name\":\"Jane\",\"age\":25}"
```

In alcuni casi potrebbe essere necessario specificare un codificatore personalizzato per la conversione in JSON. Possiamo farlo fornendo una funzione al parametro `:key-fn` di `write-str`:

```
(json/write-str person {:key-fn keyword}) ;; restituisce "{\"name\":\"Jane\",\"age\":25}"
```

Per maggiori informazioni sulle funzioni disponibili per la manipolazione di dati JSON in Clojure, controlla la documentazione della libreria `clojure.data.json`.

## Approfondimento

A volte potresti avere a che fare con JSON più complessi, che includono ad esempio un array di valori o una struttura annidata. In questi casi, puoi utilizzare le funzioni `parse-string`, `write-string` e `emit-string` di `clojure.data.json` per gestire questi casi in modo più efficiente.

Un altro aspetto da considerare quando si lavora con JSON è la gestione degli errori. Essendo un formato di dati molto flessibile, è importante avere un buon sistema di gestione degli errori per evitare problemi durante la lettura o la scrittura di file JSON.

## Vedi anche

- La documentazione ufficiale di `clojure.data.json`: https://clojure.github.io/data.json/

- Un tutorial su come lavorare con JSON in Clojure: https://purelyfunctional.tv/guide/json-in-clojure/