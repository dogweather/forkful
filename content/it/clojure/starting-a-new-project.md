---
title:                "Clojure: Iniziare un nuovo progetto"
programming_language: "Clojure"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Molti di noi amano il processo di creazione e l'emozione di un nuovo progetto. Con Clojure, abbiamo l'opportunità di unire la nostra passione per il coding con un linguaggio dinamico e potente, rendendo ogni progetto un'esperienza gratificante.

## Come fare

Prima di iniziare, assicurati di avere installato Clojure sul tuo sistema. Puoi trovarne le istruzioni su come farlo qui: [Installazione Clojure](https://clojure.org/guides/getting_started#_installation). Una volta che Clojure è installato, è il momento di creare il tuo primo progetto.

```
Clojure
(ns my-project.core)
(defn hello-world
  []
  (println "Ciao mondo!"))
(hello-world)
```

Output:
```
Ciao mondo!
```

In questo esempio, abbiamo creato un file (*my-project*) con una funzione semplice (*hello-world*) che stampa "Ciao mondo!". Per eseguire il codice, digita "lein run" nella tua shell. Il risultato dovrebbe essere la stampa del nostro messaggio di saluto.

## Approfondimento

Ci sono molti modi per iniziare un nuovo progetto in Clojure, ma un buon punto di partenza è l'utilizzo del tool "Leiningen". Questo facilita la creazione e la gestione delle dipendenze del progetto. Inoltre, consente di creare un ambiente di sviluppo consistente per il tuo progetto.

Per cominciare, esegui "lein new <project-name>" nella tua shell. Questo creerà una struttura di base per il tuo progetto, tra cui i file *project.clj* e *src/my_project/core.clj*. Il file *project.clj* contiene informazioni sul tuo progetto e le sue dipendenze, mentre il file *core.clj* è dove puoi scrivere il tuo codice.

Una volta che il tuo progetto è creato, devi impostare le dipendenze necessarie per il tuo progetto nel file *project.clj*. Questo può essere fatto utilizzando [Clojars](https://clojars.org/), un repository di librerie Clojure.

Ecco un esempio di come aggiungere una dipendenza per la popolare libreria "Ring" nel tuo progetto:

```
Clojure
(defproject my-project "0.1.0-SNAPSHOT"
  :description "Un semplice progetto Clojure"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ring "1.6.3"]])
```

Una volta impostate le dipendenze, puoi cantare il tuo progetto utilizzando il comando "lein deps". Questo scaricherà tutte le dipendenze e le sistemerà nella cartella *lib* del tuo progetto.

Ora sei pronto per iniziare a sperimentare con il tuo nuovo progetto Clojure!

## Vedi anche

- [Clojure.org - Guida all'inizio](https://clojure.org/guides/getting_started)
- [Leiningen - Documentazione](https://leiningen.org/)
- [Clojars - Repository delle librerie Clojure](https://clojars.org/)