---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

La stampa di output di debug è un modo per i programmatori di verificare il comportamento delle proprie operazioni, visualizzandole in fase di esecuzione. Lo fanno per correggere gli errori più velocemente e per avere un controllo dettagliato dell'applicazione.

## Come Fare:

Nel linguaggio Clojure, possiamo utilizzare la funzione `println` per stampare i messaggi di debug. Ecco un semplice esempio:

```Clojure
(defn hello-debug [name]
  (println "Hello, " name "! This is your debug message.")
  (println "Ciao, " name "! Questo è il tuo messaggio di debug."))
```

Dopo aver eseguito la funzione `hello-debug` con un nome, come "Mario", vedrai questo output:

```Clojure
Hello, Mario! This is your debug message.
Ciao, Mario! Questo è il tuo messaggio di debug.
```

## Approfondimento:

Historically, l'output di debug nel Clojure è stato studiato per facilitare la risoluzione dei problemi grazie alla sua semplicità. Puoi utilizzare altre librerie come `tools.logging` o `timbre` per funzionalità più avanzate, come il logging su file o l'invio di log via email.

La `println` semplicemente stampa i suoi argomenti separati da spazi sullo standard output e poi stampa una nuova riga. Questa semplice funzione può essere tutto ciò che ti serve per capire che cosa sta succedendo nel tuo codice Clojure.

## Vedi Anche:

Per un approfondimento sul debugging in Clojure, vi consigliamo le seguenti risorse:

- Clojure per il coraggioso e l'ardito: [Debugging](https://www.braveclojure.com/debugging/)
- Clojure Docs: [println](https://clojuredocs.org/clojure.core/println)
- La libreria di logging [tools.logging](https://clojure.github.io/tools.logging/)
- La libreria di logging [timbre](https://github.com/ptaoussanis/timbre)