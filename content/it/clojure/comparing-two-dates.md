---
title:    "Clojure: Confrontare due date"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Nella programmazione, è spesso necessario confrontare due date per verificare la loro relazione, ad esempio se una sia precedente o successiva all'altra. Clojure offre una semplice soluzione per la comparazione di date, che può aiutare a gestire informazioni temporali in modo efficiente.

## Come fare

Per confrontare due date in Clojure, è necessario utilizzare la funzione `before?` o `after?`. Queste funzioni accettano due parametri, rispettivamente le due date da confrontare, e restituiscono un valore booleano che indica se la prima data è precedente o successiva alla seconda.

```Clojure
(before? (java.util.Date. 2021 8 5) (java.util.Date. 2021 8 10))
;; output: true

(after? (java.util.Date. 2021 8 5) (java.util.Date. 2021 8 10))
;; output: false
```

Un'altra funzione utile è `compare`, che confronta direttamente due date e restituisce un valore numerico (-1, 0 o 1) in base alla loro relazione.

```Clojure
(compare (java.util.Date. 2021 8 5) (java.util.Date. 2021 8 10))
;; output: -1 (la prima data è precedente)

(compare (java.util.Date. 2021 8 10) (java.util.Date. 2021 8 10))
;; output: 0 (le date sono uguali)

(compare (java.util.Date. 2021 8 15) (java.util.Date. 2021 8 10))
;; output: 1 (la prima data è successiva)
```

## Approfondimento

È importante notare che le funzioni `before?` e `after?` utilizzano una comparazione `<=` e `>=` rispettivamente, mentre `compare` utilizza una comparazione `<` e `>` tra le date. Questo può portare a risultati diversi se le date sono uguali.

Inoltre, Clojure utilizza il concetto di tempo "coordinato universale" (UTC) per le date, mentre molte altre librerie usano il fuso orario locale. Ciò significa che può essere necessario convertire le date in UTC prima di confrontarle.

## Vedi anche

- Documentazione su [`before?`](https://clojuredocs.org/clojure.core/before_q)
- Documentazione su [`after?`](https://clojuredocs.org/clojure.core/after_q)
- Documentazione su [`compare`](https://clojuredocs.org/clojure.core/compare)