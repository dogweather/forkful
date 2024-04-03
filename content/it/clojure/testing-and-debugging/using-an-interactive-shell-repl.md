---
date: 2024-01-26 04:12:51.678926-07:00
description: 'Come fare: Inizia lanciando REPL.'
lastmod: '2024-03-13T22:44:43.043779-06:00'
model: gpt-4-0125-preview
summary: Inizia lanciando REPL.
title: Utilizzo di un interprete interattivo (REPL)
weight: 34
---

## Come fare:
Inizia lanciando REPL:

```Clojure
user=> (println "Ciao, REPL!")
Ciao, REPL!
nil
```

Definisci una funzione e provala:
```Clojure
user=> (defn saluta [nome] (str "Ciao, " nome "!"))
#'user/saluta
user=> (saluta "Programmatore Clojure")
"Ciao, Programmatore Clojure!"
```

Sperimenta con le strutture dati:
```Clojure
user=> (def mia-mappa {:a 1 :b 2})
#'user/mia-mappa
user=> (assoc mia-mappa :c 3)
{:a 1, :b 2, :c 3}
```

## Approfondimento
Il REPL è chiave per la filosofia di sviluppo interattivo della famiglia Lisp, e Clojure, un dialetto moderno di Lisp, sfrutta molto bene questo strumento. Risale al primo REPL Lisp alla fine degli anni '50. Alternative in altre lingue includono l'interprete Python e la console di Node.js, ma il REPL di Clojure ha uno status di prim'ordine ed è integrato nel flusso di lavoro.

Una sessione REPL di Clojure può essere integrata in vari ambienti come la riga di comando, gli IDE (come IntelliJ con Cursive, o Emacs con CIDER) o strumenti basati su browser come Nightcode. In un senso più profondo, il REPL dà potere allo sviluppatore di manipolare i costrutti del linguaggio a tempo di esecuzione e mantenere stati attraverso varie trasformazioni, spesso portando a una programmazione esplorativa e un codice più robusto.

La funzionalità del REPL brilla con strumenti come `lein repl` o `clj`, che consentono la gestione delle dipendenze, vari plugin e personalizzazioni specifiche del progetto, portando a un processo di sviluppo più produttivo e flessibile.

## Vedi Anche
- La guida ufficiale al REPL sul sito di Clojure: https://clojure.org/guides/repl/introduction
- Il discorso di Rich Hickey sullo sviluppo guidato dal REPL: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Clojure pratico: utilizzare il REPL per lo sviluppo iterativo: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
