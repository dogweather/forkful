---
title:                "Gleam: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe è un'importante tecnica di programmazione che può aiutare a manipolare e gestire grandi quantità di dati in modo più efficiente. Invece di lavorare con l'intero testo, l'estrazione di sottostringhe consente di selezionare e manipolare solo le parti pertinenti necessarie per ottenere il risultato desiderato.

## Come fare

Per estrarre sottostringhe in Gleam, utilizziamo la funzione `substring` con i parametri `start` e `length`. Ad esempio, se volessimo estrarre una sottostringa di 5 caratteri a partire dal terzo carattere di una stringa, useremmo il seguente codice:

```Gleam
let stringa = "Buongiorno!"

let sottostringa = stringa.substring(start: 3, length: 5)

io.print(sottostringa) // output: ngior
```

Il codice sopra selezionerà i caratteri dalla posizione 3 alla posizione 7 (5 caratteri) e li assegnerà alla variabile `sottostringa`. Nota che le posizioni dei caratteri iniziano a contare da 0.

## Approfondimento

Oltre ai parametri `start` e `length`, la funzione `substring` in Gleam ha anche un terzo parametro opzionale chiamato `stop`. Questo parametro permette di estrarre una sottostringa utilizzando un indice di fine invece di una lunghezza. Ad esempio, il seguente codice otterrà lo stesso risultato del codice precedente:

```Gleam
let stringa = "Buongiorno!"

let sottostringa = stringa.substring(start: 3, stop: 7)

io.print(sottostringa) // output: ngior
```

Inoltre, la funzione `substring` può essere utilizzata per estrarre sia da stringhe che da liste di caratteri.

## Vedi anche

- Documentazione Gleam su `substring`: https://gleam.run/modules/gleam_stdlib/string/#substring
- Tutorial su come utilizzare le funzioni di manipolazione delle stringhe in Gleam: https://gleam.run/articles/strings/