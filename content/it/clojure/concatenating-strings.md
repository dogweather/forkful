---
title:    "Clojure: Capire Concatenazione di Stringhe"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione molto comune in tutti i linguaggi di programmazione, inclusa Clojure. Combina più stringhe in una singola stringa più lunga e può essere utile in diverse situazioni, come la creazione di output per l'utente o la costruzione di URL dinamici per richieste HTTP.

## Come fare

Per concatenare stringhe in Clojure, possiamo utilizzare la funzione `str`, che accetta uno o più argomenti di tipo stringa. Esempio:

```Clojure
(str "Ciao" " " "Mondo") 

;; Output: "Ciao Mondo"
```

Possiamo anche utilizzare l'operatore di concatenazione `str` o il metodo `concat` dalla libreria di base di Clojure:

```Clojure
"Hello" str "World"
(concat "Hello" "World")

;; Output: "Hello World"
```

Inoltre, possiamo utilizzare la funzione `format` per concatenare stringhe con formattazione e argomenti. Esempio:

```Clojure
(format "Ciao %s, hai %d anni" "Mario" 35)

;; Output: "Ciao Mario, hai 35 anni"
```

## Approfondimento

Clojure utilizza l'implementazione `java.lang.StringBuilder` per gestire la concatenazione di stringhe, che rende più efficiente l'operazione rispetto ad altri linguaggi che creano nuove stringhe ogni volta che viene eseguita la concatenazione.

Inoltre, è importante ricordare che le stringhe in Clojure sono immutabili, il che significa che ogni nuova operazione di concatenazione crea una nuova stringa, quindi è consigliabile evitarne l'uso in situazioni in cui è richiesta un'alta performance.

## Vedi anche

- [String Functions in Clojure](https://clojuredocs.org/clojure.string)
- [Clojure Cheatsheet: Strings](https://clojure.org/api/cheatsheet)
- [Java StringBuilder](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)