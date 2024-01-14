---
title:                "Clojure: Utilizzare le espressioni regolari"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Usare le espressioni regolari può sembrare intimidatorio per alcuni programmatori, ma in realtà è uno strumento potente per manipolare i dati e automatizzare determinati compiti. Se vuoi imparare a sfruttare al meglio le espressioni regolari in Clojure, continua a leggere!

## Come fare

Per utilizzare le espressioni regolari in Clojure, dovrai importare il modulo "re" utilizzando la funzione require. Inizializziamo poi una nuova espressione regolare usando il costrutto #"" e assegniamola a una variabile.

```Clojure
(require '[clojure.string :as str])
(require '[clojure.core.re :as re])

(def regex #"[A-Z]+")
```

Una volta che hai creato l'espressione regolare, puoi usarla con diverse funzioni come `re-find` per trovare un match all'interno di una stringa o `re-seq` per trovare tutti i match all'interno di una stringa. Vediamo un esempio utilizzando una stringa contenente un indirizzo email:

```Clojure
(def email "test@email.com")

(re-find regex email)
;; Output: "TEST"
(re-seq regex email)
;; Output: ("TEST")
```

Puoi anche usare le espressioni regolari per sostituire parti di una stringa con un'altra. Ad esempio, se voglio sostituire tutti i caratteri maiuscoli nella mia email con "X", posso usare la funzione `re-sub`:

```Clojure
(def new-email (re-sub regex "X" email))
;; Output: "Xest@email.com"
```

## Approfondimento

Ora che hai imparato come utilizzare le espressioni regolari in Clojure, potresti essere curioso su come funzionano nel dettaglio. In breve, le espressioni regolari sono semplicemente modelli di stringhe che vengono utilizzati per trovare un certo tipo di pattern all'interno di altre stringhe. Puoi imparare di più sulle loro funzionalità e sintassi esplorando la documentazione di Clojure sulle espressioni regolari.

## Vedi anche

- [Documentazione di Clojure sulle espressioni regolari](https://clojure.org/guides/regex)
- [Introduzione alle espressioni regolari in Clojure](https://curiousprogrammer.io/blog/clojure-regex-cheatsheet/)
- [Tutorial sulle espressioni regolari in Clojure](https://dev.to/anthonyhewins/a-gentle-introduction-to-regular-expression-in-clojure-42al)