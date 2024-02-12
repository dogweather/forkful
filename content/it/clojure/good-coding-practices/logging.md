---
title:                "Registrazione Eventi (Logging)"
aliases: - /it/clojure/logging.md
date:                  2024-01-26T01:01:11.287933-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione Eventi (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/logging.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Il logging è essenzialmente l'equivalente software di un registro di bordo di una nave; è un modo per registrare gli eventi che si verificano mentre un'applicazione è in esecuzione. I programmatori lo fanno per tenere traccia di questi eventi per il debug, le tracce di audit o per ottenere intuizioni sul comportamento di un sistema in produzione.

## Come fare:
Clojure si appoggia alle funzionalità di logging di Java, ma è possibile accedervi in un modo più idiomatico per Clojure. Vediamo come potresti utilizzare `clojure.tools.logging`, che fornisce una semplice astrazione su vari framework di logging:

Prima di tutto, aggiungi una dipendenza per `clojure.tools.logging` e un'implementazione di logging come `log4j` nel tuo `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Ora, registrare alcuni messaggi:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Inizio calcolo intenso...")
  (Thread/sleep 3000) ; Simula un lungo calcolo
  (log/info "Calcolo completato. La risposta è 42.")
  42)

(compute-answer-to-everything)
```
L'output non mostrerà i messaggi `DEBUG` per impostazione predefinita, dato che i livelli di log sono tipicamente impostati su `INFO`:

```
INFO  [your-namespace] - Calcolo completato. La risposta è 42.
```

È possibile configurare i livelli di log e gli appender in un file `log4j.properties` per ottenere un output più verboso se necessario.

## Approfondimento
Il `clojure.tools.logging` di Clojure esiste da un po' di tempo e funge da ponte tra il codice Clojure e il mondo del logging Java. Storicamente, Java ha attraversato diverse iterazioni e librerie per il logging come l'API di logging integrata di Java, `log4j`, `slf4j`, e `logback`.

In Clojure, mentre è possibile utilizzare direttamente i framework di logging Java, `clojure.tools.logging` rileva e delega a qualsiasi framework di logging trovi nel tuo classpath, evitandoti di essere strettamente legato a un'implementazione specifica. Questo può aiutare a mantenere il tuo codice Clojure più portatile e modulare.

Le alternative a `clojure.tools.logging` nell'ecosistema Clojure includono librerie come `timbre`, che è una libreria di logging pura Clojure con funzionalità come la rotazione dei log, il filtraggio e il logging asincrono già pronti per l'uso.

I dettagli dell'implementazione sono cruciali quando si tratta di logging in un ambiente multithread come Clojure. Qui, l'immutabilità e la gestione degli effetti collaterali forniscono vantaggi distinti. Il logging, come effetto collaterale, dovrebbe essere gestito con cura per evitare colli di bottiglia nelle prestazioni e garantire la sicurezza tra i thread, di cui la maggior parte dei framework di logging Java si occupa già.

Infine, considera il logging strutturato, dove i log vengono scritti come dati strutturati (come JSON). Questo può essere estremamente utile per analisi e processamenti successivi, specialmente quando si hanno a che fare con sistemi distribuiti su larga scala.

## Vedi Anche
Se hai voglia di approfondire, considera di consultare queste risorse:

- Documentazione di Clojure Tools Logging: https://github.com/clojure/tools.logging
- Timbre, una libreria di logging per Clojure: https://github.com/ptaoussanis/timbre
- Configurazione di Log4J in Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Manuale di Logback per configurazioni avanzate: http://logback.qos.ch/manual/
- Una guida sul logging strutturato in Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
