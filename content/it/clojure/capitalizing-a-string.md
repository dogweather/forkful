---
title:    "Clojure: Capitalizzazione di una stringa"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è utile quando si vuole dare enfasi o uniformità al testo. Ad esempio, può essere utile nel caso di titoli o nomi propri.

## Come Fare

Per capitalizzare una stringa in Clojure, possiamo utilizzare la funzione `clojure.string/capitalize`. Questa funzione prende come argomento una stringa e restituisce la stessa stringa con la prima lettera maiuscola.

```Clojure
(clojure.string/capitalize "ciao mondo") ; Output: "Ciao mondo"
```

Possiamo anche capitalizzare ogni parola di una stringa utilizzando la funzione `clojure.string/capitalize-words`.

```Clojure
(clojure.string/capitalize-words "ciao a tutti") ; Output: "Ciao A Tutti"
```

## Approfondimento

La funzione `clojure.string/capitalize` utilizza il metodo `toUpperCase` di Java per capitalizzare la prima lettera. Questo significa che funziona solo con caratteri ASCII e non funzionerà con caratteri speciali o lettere accentate.

Se vogliamo essere più precisi e gestire anche questi casi, possiamo utilizzare la libreria `clojure.string.upper-case`.

```Clojure
(clojure.string.upper-case "ciao à tutti") ; Output: "Ciao À Tutti"
```

È importante notare che entrambe le funzioni non modificano la stringa originale, ma restituiscono una nuova stringa con la formattazione corretta.

## Vedi Anche

- [Documentazione di `clojure.string`](https://clojure.github.io/clojure/clojure.string-api.html)
- [Documentazione di `clojure.string.upper-case`](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/upper-case)