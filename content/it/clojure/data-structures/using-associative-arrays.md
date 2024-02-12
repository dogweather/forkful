---
title:                "Utilizzo di array associativi"
aliases:
- /it/clojure/using-associative-arrays.md
date:                  2024-01-30T19:10:20.015855-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di array associativi"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Gli array associativi, o mappe hash, in Clojure consentono di memorizzare e recuperare dati tramite coppie chiave-valore. Sono la scelta preferita per la gestione di dati strutturati, rendendo più rapido l'accesso a elementi specifici senza dover iterare attraverso una lista.

## Come fare:

In Clojure, creare e manipolare array associativi (mappe hash) è semplice. Vediamo alcuni esempi.

Per creare una mappa hash:

```clojure
(def my-map {:name "Alex" :age 30})
```

È possibile recuperare un valore specificando la sua chiave:

```clojure
(get my-map :name)
;; "Alex"
```
Oppure, più idiomaticamente, si può usare la chiave come una funzione:

```clojure
(:name my-map)
;; "Alex"
```

Aggiungere o aggiornare voci è semplice:

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

Per rimuovere chiavi, utilizzare `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

Per iterare su una mappa:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

E per un accesso condizionale, `find` restituisce una coppia chiave-valore se la chiave esiste:

```clojure
(find my-map :age)
;; [:age 30]
```

## Approfondimento

Gli array associativi in Clojure, comunemente noti anche come mappe hash, sono incredibilmente versatili ed efficienti per la gestione di dati basati su coppie chiave-valore. Fanno parte della ricca libreria di collezioni di Clojure, profondamente radicata nella filosofia del linguaggio dell'immutabilità e della programmazione funzionale. A differenza degli array o delle liste che richiedono una complessità temporale O(n) per l'accesso agli elementi, le mappe hash forniscono una complessità temporale quasi costante per l'accesso, rendendole altamente efficienti per le operazioni di ricerca.

Si potrebbe sostenere che i vettori in Clojure potrebbero servire a uno scopo simile attraverso l'accesso indicizzato, ma le mappe hash eccellono quando si tratta di gestire dati non sequenziali e etichettati, dove la chiave fornisce un descrittore significativo piuttosto che un indice arbitrario.

Unico nel suo genere in Clojure (e nella sua eredità Lisp), gli array associativi sono cittadini di prima classe, il che significa che possono essere manipolati direttamente, passati tra funzioni e altro, senza bisogno di sintassi speciali o metodi di accesso. Questa decisione di progettazione rafforza l'enfasi di Clojure su semplicità e potenza.

Sebbene le mappe hash siano incredibilmente utili, vale la pena menzionare che per set di dati molto grandi o scenari in cui le chiavi sono altamente dinamiche (aggiunta e rimozione costanti), strutture di dati alternative o database potrebbero offrire migliori prestazioni e flessibilità. Tuttavia, per la maggior parte dei casi d'uso tipici nell'ambito delle applicazioni Clojure, gli array associativi forniscono un mezzo robusto ed efficiente per la gestione dei dati.
