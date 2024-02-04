---
title:                "Capitalizzare una stringa"
date:                  2024-02-03T19:04:51.037331-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizzare una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?
Capitalizzare una stringa comporta modificare la stringa in modo che il suo primo carattere sia in maiuscolo, mentre il resto della stringa rimane invariato. I programmatori spesso eseguono la capitalizzazione delle stringhe per garantire la coerenza dei dati, specialmente per nomi e luoghi, o per conformarsi alle regole grammaticali nelle interfacce utente.

## Come fare:
Clojure, essendo un linguaggio JVM, ti permette di utilizzare direttamente i metodi String di Java. Ecco un esempio base di come capitalizzare una stringa in Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure non include una funzione incorporata specificamente per capitalizzare le stringhe, ma come mostrato, è possibile raggiungere facilmente questo risultato combinando le funzioni `clojure.string/upper-case`, `subs` e `str`.

Per una soluzione più concisa e per gestire manipolazioni di stringhe più complesse, potresti rivolgerti a una libreria di terze parti. Una libreria popolare nell'ecosistema di Clojure è `clojure.string`. Tuttavia, al momento del mio ultimo aggiornamento, non offre una funzione `capitalize` diretta oltre a quanto dimostrato con le funzionalità di base di Clojure, quindi il metodo mostrato sopra è il tuo approccio diretto senza dover includere librerie aggiuntive specificamente per la capitalizzazione.

Ricorda, quando lavori con stringhe in Clojure che interagiscono con metodi Java, stai effettivamente lavorando con stringhe Java, permettendoti di sfruttare l'intero arsenale dei metodi String di Java direttamente nel tuo codice Clojure, se necessario.
