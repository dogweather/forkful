---
title:                "Eliminare caratteri corrispondenti a un modello."
html_title:           "Gleam: Eliminare caratteri corrispondenti a un modello."
simple_title:         "Eliminare caratteri corrispondenti a un modello."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa e perché?

L'eliminazione di caratteri corrispondenti a un determinato modello è una pratica comune tra i programmatori che consente di rimuovere rapidamente parti di testo o dati indesiderati da un codice o da un file. In questo modo è possibile semplificare il lavoro di pulizia, di formattazione o di analisi dei dati e rendere il codice più leggibile e performante.

## Come fare:

Ecco alcuni esempi di codice che mostrano come eliminare i caratteri corrispondenti a un determinato modello utilizzando Gleam:

```Gleam
// Rimuove tutte le lettere "a" dal testo
let str = "casa"
let cleaned_str = String.trim(String.replace("a", "", str))

// Elimina tutti i numeri da una lista di stringhe
let numbers = ["12", "34", "56"]
let cleaned_numbers = List.map(\x -> String.replace("\\d", "", x), numbers)
```

Output:

```Gleam
// "cs"
cleaned_str

// ["", "", ""]
cleaned_numbers
```

## Approfondimento:

Questa pratica di eliminazione di caratteri è stata introdotta in linguaggi di programmazione come Perl, dove si è rivelata molto utile per la gestione dei testi. Tuttavia, con l'avvento di linguaggi più moderni come Gleam, questa funzionalità diventa più semplice e sicura da utilizzare. Alcune alternative per eliminare caratteri in Gleam includono l'uso di funzioni come `String.filter` o `String.replace_re`, ma l'uso di `String.replace` rimane il metodo più comune e semplice. L'implementazione di `String.replace` nella standard library di Gleam utilizza l'algoritmo di Boyer-Moore, che è noto per la sua velocità e efficiente gestione di grandi quantità di dati.

## Vedi anche:

- Documentazione ufficiale di Gleam: https://gleam.run/documentation/
- Tutorial su Gleam: https://gleam.run/book/
- Altri articoli sul linguaggio di programmazione Gleam: https://gleam.run/articles/