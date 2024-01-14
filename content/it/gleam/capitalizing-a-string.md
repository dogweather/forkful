---
title:                "Gleam: Trasformare una stringa in maiuscolo"
simple_title:         "Trasformare una stringa in maiuscolo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa può sembrare un semplice compito, ma in realtà può risultare molto utile quando si lavora con dati e stringhe in un programma. Può rendere più leggibile il testo e facilitare la comprensione delle informazioni.

## Come Fare

Per capitalizzare una stringa in Gleam, è possibile utilizzare la funzione built-in `String.capitalize/1`. Questa funzione accetta una stringa come argomento e restituisce una nuova stringa dove il primo carattere è in maiuscolo.

Ecco un esempio di come utilizzarla:

```Gleam
let stringa = "ciao mondo"

let nuova_stringa = String.capitalize(stringa)

// Output: "Ciao mondo"
```

Si noti che la funzione `capitalize/1` non modifica la stringa originale, ma restituisce una nuova stringa capitalizzata.

## Approfondimento

Oltre alla funzione `capitalize/1`, esistono altre tecniche per capitalizzare una stringa in Gleam. Ad esempio, è possibile utilizzare la funzione `String.to_title_case/1` per capitalizzare non solo il primo carattere, ma anche il primo carattere di ogni parola nella stringa.

Inoltre, Gleam fornisce moduli per il parsing e la manipolazione dei dati di testo più avanzati, come `String.CaseConversion` e `String.Transforms`. Questi possono essere utili quando si lavora con stringhe più complesse e vanno oltre la semplice capitalizzazione.

## Vedi Anche

- Documentazione di Gleam su `String.capitalize/1`: https://gleam.run/modules/string.html#capitalize
- Esempi di codice di Gleam: https://github.com/gleam-lang/gleam/tree/master/examples
- Altre funzioni di manipolazione delle stringhe in Gleam: https://gleam.run/modules/string.html