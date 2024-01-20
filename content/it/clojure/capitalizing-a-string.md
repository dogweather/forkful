---
title:                "Mettere in Maiuscolo una Stringa"
html_title:           "Clojure: Mettere in Maiuscolo una Stringa"
simple_title:         "Mettere in Maiuscolo una Stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché? 

La capitalizzazione di una stringa in programmazione significa convertire la prima lettera di ogni parola in maiuscolo. I programmatori lo fanno spesso per migliorare la leggibilità e la presentazione dei dati, per esempio, nella generazione di report o nell'interfaccia utente.

## Come fare:

Ecco un esempio di come puoi capitalizzare una stringa in Clojure:

```Clojure
(defn capitalize [s]
  (->> (clojure.string/split s #" ")
       (map clojure.string/capitalize)
       (clojure.string/join " ")))

(println (capitalize "ciao mondo!"))  ; Stampa: "Ciao Mondo!"
```

In questo codice, la funzione `capitalize` divide la stringa in parole, usa `map` per applicare `clojure.string/capitalize` su ogni parola, e poi usa `join` per ricongiungerle in una stringa unica.

## Approfondimenti:

1. **Contesto storico**: La funzionalità di capitalizzazione delle stringhe esiste da molto tempo nella programmazione, da quando le interfacce utente basate su testo richiedevano un modo per migliorare la leggibilità dei dati.

2. **Alternative**: In alternativa, possiamo usare la funzione `clojure.string/upper-case` se vogliamo trasformare tutte le lettere di una stringa in maiuscole, non solo la prima lettera di ogni parola.

3. **Dettagli di implementazione**: `clojure.string/capitalize` è una funzione di aiuto che prima converte tutte le lettere in minuscolo usando `lower-case` e poi converte solo la prima lettera in maiuscolo.

## Vedi Anche:

Per ulteriori informazioni sulle funzioni di manipolazione delle stringhe in Clojure, dai un'occhiata a questi collegamenti:

- [clojure.string API](https://clojuredocs.org/clojure.string) fornisce un elenco completo delle funzioni di stringa supportate in Clojure.
- [Capitalization in programming](https://stackoverflow.com/questions/409533/capitalization-in-programming) discute l'uso e il significato della capitalizzazione nelle stringhe di programmazione in diversi linguaggi.