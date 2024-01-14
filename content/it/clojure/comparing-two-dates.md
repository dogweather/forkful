---
title:                "Clojure: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti dover confrontare due date in un programma Clojure. Ad esempio, potresti aver bisogno di verificare se una data è antecedente o successiva rispetto a un'altra, o di calcolare la differenza tra due date. Conoscere come confrontare due date in modo efficace può semplificare la gestione delle date nel tuo codice e aiutarti a evitare errori.

## Come fare

Per confrontare due date in Clojure, puoi utilizzare la funzione `compare`, che prende in input due date e restituisce -1 se la prima è precedente alla seconda, 0 se sono uguali o 1 se la prima è successiva alla seconda. Ecco un esempio di codice:

```Clojure
(def data1 (java.util.Date. 2021 6 1))
(def data2 (java.util.Date. 2021 6 15))

(println (compare data1 data2))
```

L'output di questo codice sarà `1`, poiché la data1 è successiva alla data2. Puoi anche utilizzare la funzione `before?` o `after?` per verificare se una data è antecedente o successiva rispetto a un'altra, come mostrato in questo esempio:

```Clojure
(def data1 (java.util.Date. 2021 6 1))
(def data2 (java.util.Date. 2021 6 15))

(println (before? data1 data2))
(println (after? data1 data2))
```

L'output di questo codice sarà rispettivamente `true` e `false`, poiché la data1 è antecedente alla data2.

## Approfondimento

Ci sono alcune cose importanti da considerare quando si confrontano due date in Clojure. Innanzitutto, è necessario assicurarsi che le date siano nello stesso formato prima di utilizzare la funzione `compare`. Inoltre, è importante considerare che la funzione `compare` confronta le date utilizzando sia il giorno che l'ora, quindi se desideri confrontare solo la data senza tener conto dell'ora, puoi utilizzare la funzione `equal?`.

Un altro aspetto da tenere a mente è che le date in Clojure sono rappresentate come oggetti mutabili. Ciò significa che se modifichi una data, anche altre funzioni che la utilizzano potrebbero essere influenzate. Assicurati quindi di gestire correttamente le date e di creare nuovi oggetti quando necessario.

## Vedi anche

- Documentazione ufficiale di Clojure sulla funzione `compare`: https://clojuredocs.org/clojure.core/compare
- Tutorial su come gestire le date in Clojure: https://clojure-doc.org/articles/ecosystem/java_jdbc/home.html
- Altri esempi di codice su come confrontare date in Clojure: https://github.com/jafingerhut/clojure-date-utils/blob/master/src/bbatsov/date_utils.clj