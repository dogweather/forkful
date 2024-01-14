---
title:    "Clojure: Confrontare due date"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è un'operazione comune nella programmazione, soprattutto quando si lavora con dati temporali o si devono effettuare operazioni di ordinamento. Imparare come fare un confronto tra due date può essere molto utile per la scrittura di codice più intuitivo e efficiente.

## Come fare

Per confrontare due date in Clojure, possiamo utilizzare la funzione `compare`, che restituisce 0 se le due date sono uguali, -1 se la prima è precedente alla seconda e 1 se la prima è successiva alla seconda.

```
Clojure
(let [date1 (java.sql.Date. 2021 7 5)
      date2 (java.sql.Date. 2021 7 15)]
  (println (compare date1 date2)))
```

In questo esempio, confrontiamo due date utilizzando la funzione `compare`, e il risultato stampato in output sarà -1, poiché la prima data è precedente alla seconda.

Possiamo anche utilizzare gli operatori `>` e `<` per confrontare le date in un modo più intuitivo:

```
Clojure
(let [date1 (java.sql.Date 2021 7 5)
      date2 (java.sql.Date. 2021 7 15)]
  (println (date1 > date2)))
```

In questo caso, il risultato stampato sarà `false`, poiché la prima data è precedente alla seconda.

## Approfondimento

Quando si confrontano date, è importante conoscere il loro formato e come vengono rappresentate in Clojure. Le date vengono rappresentate come oggetti di tipo `java.sql.Date`, che possono essere creati utilizzando la classe `java.sql.Date` e fornendo i parametri corretti per l'anno, il mese e il giorno.

Inoltre, quando si confrontano date, è fondamentale considerare anche il fuso orario in cui si trovano le date. Ad esempio, se si confrontano due date con fusi orari diversi, è possibile ottenere un risultato inatteso.