---
title:    "Clojure: Trasformare una data in una stringa"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è una delle operazioni comuni durante la programmazione in Clojure. Potresti aver bisogno di farlo per visualizzare la data in un formato specifico o per salvarla in un file di testo. In questo articolo, esploreremo come farlo in modo efficiente utilizzando alcune funzioni di base di Clojure.

## Come fare

Per convertire una data in una stringa, dobbiamo utilizzare la funzione `str` che prende in input la data e la converte in una stringa. Possiamo anche specificare il formato della stringa utilizzando la funzione `format`.

```
Clojure (str (LocalDate/of 2021 10 25))
```

Output:
" 2021-10-25 "

Possiamo anche specificare il formato utilizzando la funzione `format`.

```
(clojure.string/format "%d/%m/%Y" (LocalDate/of 2021 10 25))
```

Output:
" 25/10/2021 "

## Approfondimento

Quando si utilizzano le funzioni `str` o `format`, è importante considerare il tipo di dato che viene passato come parametro. Ad esempio, se passiamo una data come parametro, Clojure la converte automaticamente in un oggetto `LocalDate` che possiamo manipolare ulteriormente. Se abbiamo bisogno di specificare una data nel formato "gg/mm/aaaa", dobbiamo prima convertirla in un oggetto `LocalDate` e poi utilizzare la funzione `format` per ottenere il formato desiderato.

## Vedi anche

- [Documentazione ufficiale di Clojure](https://clojure.org/index)
- [Altre funzioni di conversione in Clojure](https://clojuredocs.org/clojure.pprint/print-datetime)
- [Esempi di conversione di una data in stringa in Clojure](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/09_data/9-02_dates.asciidoc)