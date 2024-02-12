---
title:                "Rifattorizzazione"
aliases: - /it/clojure/refactoring.md
date:                  2024-01-26T01:17:35.293870-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rifattorizzazione"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/refactoring.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il refactoring è il processo di ristrutturazione del codice informatico esistente senza cambiare il suo comportamento esterno, mirato a migliorare attributi non funzionali. I programmatori rifattorizzano per rendere il loro codice più pulito, più efficiente e più facile da mantenere, migliorando effettivamente la leggibilità e riducendo la complessità del loro software.

## Come fare:

Il refactoring in Clojure—grazie alla sua sintassi pulita e al paradigma funzionale—può essere incredibilmente diretto. Affrontiamo uno scenario comune: iterare sulle collezioni. Potresti iniziare con un ciclo `for`, cosi:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Chiamando `(old-way)` otterremo 55, la somma da 1 a 10. Ma, ehi, possiamo rifattorizzare questo per essere più Clojure-esque:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Questa funzione `(new-way)` rifattorizzata utilizza le macro di threading per passare il range direttamente a `reduce`, tagliando il grasso in eccesso.

## Approfondimento

L'arte del refactoring ha le sue radici nei primi giorni dello sviluppo del software ma ha realmente guadagnato slancio con il libro fondamentale di Martin Fowler "Refactoring: Improving the Design of Existing Code" pubblicato nel 1999. In Clojure, il refactoring spesso si appoggia ai principi della programmazione funzionale, privilegiando funzioni pure e strutture dati immutabili.

Alternative al refactoring manuale in Clojure potrebbero includere l'uso di strumenti come Cursive, un popolare plugin di IntelliJ IDEA, che offre rifattorizzazioni automatizzate specifiche per Clojure. C'è anche clj-refactor, un pacchetto Emacs per Clojure, che fornisce una suite di funzioni di rifattorizzazione.

Una sfida particolare del refactoring in Clojure è la gestione dello stato e degli effetti collaterali in un paradigma principalemente immutabile e libero da effetti collaterali. L'uso attento di atomi, riferimenti, agenti e transitori è fondamentale nel mantenere sia la prestazione che la correttezza durante i refactoring.

## Vedi Anche

- "Refactoring: Improving the Design of Existing Code" di Martin Fowler per i concetti fondamentali.
- [Clojure Docs](https://clojuredocs.org/) per esempi specifici di codice Clojure idiomatico.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) per l'automazione del refactoring in Emacs.
- [Cursive](https://cursive-ide.com/) per gli utenti IntelliJ che cercano assistenza automatizzata nel refactoring.
- [Refactoring con Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - Una conferenza del creatore di Clojure che, sebbene non riguardi specificamente il refactoring, fornisce intuizioni sulla filosofia di Clojure che possono guidare decisioni di refactoring efficaci.
