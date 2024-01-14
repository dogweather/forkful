---
title:                "Clojure: Capitalizzazione di una stringa"
simple_title:         "Capitalizzazione di una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

La capitalizzazione di una stringa è un'operazione comune nella programmazione. Può essere utile per rendere una sicuro utente ottenendo dati di input, per normalizzare l'output di un algoritmo o semplicemente per rendere più leggibile una stringa.

## Come fare

Una delle opzioni più semplici per capitalizzare una stringa in Clojure è utilizzare la funzione `clojure.string/capitalize`. Questa funzione prende in input una stringa e restituisce una nuova stringa con la prima lettera maiuscola. Ad esempio:

```Clojure
(clojure.string/capitalize "ciao a tutti")
```

Questo produrrà l'output `"Ciao a tutti"`.

Inoltre, se si vuole capitalizzare tutte le parole all'interno di una stringa, si può utilizzare la funzione `clojure.string/capitalize-words`, come nell'esempio seguente:

```Clojure
(clojure.string/capitalize-words "ciao a tutti")
```

Questo produrrà l'output `"Ciao A Tutti"`.

## Approfondimento

La funzione `clojure.string/capitalize` utilizza internamente la funzione `clojure.string/lower-case` per convertire tutte le lettere della stringa in minuscolo e poi la funzione `clojure.string/upper-case` per trasformare la prima lettera in maiuscolo. Se si vuole creare una funzione personalizzata per capitalizzare una stringa, si può utilizzare questo approccio come punto di partenza.

## Vedi anche

- [Documentazione ufficiale di Clojure per la stringa di funzioni](https://clojure.github.io/clojure/clojure.string-api.html)
- [Guida di Clojure per iniziare con le stringhe](https://clojure.org/guides/learn/strings)
- [Altro post del blog sulla manipolazione delle stringhe in Clojure](https://dev.to/cdli/clojure-a-language-of-the-demos-beautiful-string-manipulation-using-regexes-1lp9)