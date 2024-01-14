---
title:                "Clojure: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo può essere utile per rendere uniformi le stringhe in un programma o per confrontare stringhe senza distinzione tra maiuscole e minuscole.

## Come fare

Per convertire una stringa in minuscolo in Clojure, possiamo utilizzare la funzione `lower-case` della libreria standard `clojure.string`.

```Clojure
(clojure.string/lower-case "HELLO")
```

L'output di questo codice sarà `"hello"`.

Possiamo anche utilizzare la funzione `str/lower-case` dalla libreria `clojure.string` che è basata sul codice Java sottostante.

```Clojure
(require '[clojure.string :as str])

(str/lower-case "Hello")
```

In entrambi i casi, l'output sarà sempre una stringa in minuscolo.

## Approfondimento

Quando si utilizza `lower-case` o `str/lower-case`, è possibile specificare una locale come secondo argomento, in modo da gestire correttamente le stringhe in lingue diverse. Ad esempio, se vogliamo convertire la stringa "Hola" in minuscolo con la locale spagnola, possiamo scrivere:

```Clojure
(str/lower-case "Hola" (java.util.Locale. "es"))
```

Questo produrrà l'output `"hola"`.

È importante notare che la funzione `lower-case` non effettua alcuna modifica sulla stringa iniziale, piuttosto restituisce una nuova stringa in minuscolo. Questo è particolarmente importante quando si lavora con stringhe immutabili in Clojure.

## Vedi anche

- [ClojureDocs - lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [ClojureDocs - locale](https://clojuredocs.org/java.util.Locale)