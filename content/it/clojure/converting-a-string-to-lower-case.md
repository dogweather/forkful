---
title:                "Clojure: Convertire una stringa in minuscolo"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui uno potrebbe voler convertire una stringa in lettere minuscole durante la programmazione in Clojure. Alcuni esempi potrebbero includere la necessità di confrontare due stringhe, di semplificare un output o di risolvere problemi di capitalizzazione in un programma.

## Come fare

Ci sono diversi modi per convertire una stringa in lettere minuscole in Clojure. Ecco tre metodi comuni:

```Clojure
(.toLowerCase "PAROLA") ; restituirà "parola"
```

```Clojure
(lower-case "FRASE INTERA") ; restituirà "frase intera"
```

```Clojure
(.toLowerCase "TESTO" java.util.Locale/ITALIAN) ; restituirà "testo"
```

In questi esempi, stiamo utilizzando la funzione ".toLowerCase" e la funzione predefinita "lower-case" per convertire le stringhe in lettere minuscole. Nota che nella terza opzione stiamo anche specificando la lingua italiana nella quale vogliamo che venga convertita la stringa.

## Profondità

Con oggi, abbiamo parlato di alcuni modi per convertire una stringa in lettere minuscole in Clojure. Tuttavia, è importante ricordare che la funzione ".toLowerCase" funziona solo con le stringhe in formato Java. Se hai bisogno di lavorare con stringhe Clojure, dovrai utilizzare la funzione "lower-case" o convertire prima la stringa in formato Java.

È anche importante notare che la funzione ".toLowerCase" può causare problemi con l'encoding dei caratteri in alcune situazioni. In questi casi, è consigliabile utilizzare la funzione "lower-case" invece.

## Vedi anche

Per ulteriori informazioni sulle funzioni di manipolazione delle stringhe in Clojure, assicurati di dare un'occhiata a questi siti:

- https://clojuredocs.org/clojure.string/lower-case
- https://clojuredocs.org/clojure.string/.toLowerCase
- http://clojurekatas.org/string-manipulation/#lower-case