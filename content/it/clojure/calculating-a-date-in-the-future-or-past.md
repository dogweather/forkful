---
title:                "Clojure: Calcolare una data nel futuro o nel passato"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere necessario per molti motivi, come la pianificazione di eventi o la gestione di appuntamenti. La programmazione in Clojure offre strumenti potenti per eseguire questi calcoli con facilità e precisione.

## Come fare

Per calcolare una data nel futuro o nel passato in Clojure, possiamo utilizzare la funzione `clj-time.core/plus` e `clj-time.core/minus`. Queste funzioni accettano tre argomenti: una data di partenza, un intervallo di tempo e una stringa che specifica l'unità di tempo (giorni, settimane, mesi, etc.).

Ecco un esempio del codice per ottenere la data di oggi più due settimane:

```Clojure
(require '[clj-time.core :as time])

(def oggi (time/today))
(def due-settimane (time/days 14))
(def nuova-data (time/plus oggi due-settimane :days))

(println nuova-data)
; => #inst"2021-04-26T00:00:00.000000000-00:00"
```

Possiamo anche specificare una data specifica come punto di partenza e ottenere una data nel passato utilizzando la funzione `clj-time.core/minus`. Ecco un esempio del codice per ottenere la data del 1° gennaio 2021 meno 3 mesi:

```Clojure
(def inizio-2021 (time/date 2021 1 1))
(def tre-mesi (time/months -3))
(def data-passata (time/minus inizio-2021 tre-mesi :months))

(println data-passata)
; => #inst"2020-10-01T00:00:00.000000000-00:00"
```

## Approfondimento

Per eseguire calcoli più complessi per date nel futuro o nel passato in Clojure, possiamo utilizzare la libreria `clj-time`, che fornisce una sintassi più semplice e intuitiva per lavorare con le date. Possiamo anche utilizzare la funzione `clj-time.core/interval` per specificare un intervallo di tempo più preciso, come giorni lavorativi o ore.

Ad esempio, possiamo calcolare la data di oggi più 5 giorni lavorativi utilizzando la libreria `clj-time`:

```Clojure
(require '[clj-time.core :as time]
         '[clj-time.periodic :as periodic])

(def oggi (time/today))
(def cinque-giorni-lavorativi (periodic/hours (periodic/weeks 1 :days "mon,tue,wed,thu,fri")))
(def nuova-data (time/plus oggi cinque-giorni-lavorativi :hours))

(println nuova-data)
; => #inst"2021-04-23T00:00:00.000000000-00:00"
```

Utilizzando la libreria `clj-time`, possiamo anche convertire facilmente le date in vari formati, come stringhe o oggetti JodaTime.

## Vedi anche
- [Funzioni di calcolo di date in Clojure](https://cljdoc.org/d/clj-time/clj-time/0.15.2/api/clj-time.core#plus)
- [Libreria clj-time](https://github.com/clj-time/clj-time)
- [Documentazione su JodaTime](https://www.joda.org/joda-time/)