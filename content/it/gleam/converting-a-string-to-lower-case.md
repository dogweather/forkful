---
title:                "Converting una stringa in minuscolo"
html_title:           "Gleam: Converting una stringa in minuscolo"
simple_title:         "Converting una stringa in minuscolo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Converting un stringa a minuscolo è importante quando si lavora con dati sensibili come password o codici di riferimento, in quanto ci assicura che non ci siano errori di input dovuto a diversità nelle lettere maiuscole e minuscole.

## Come fare
È molto semplice convertire una stringa a minuscolo in Gleam. Basta seguire questi passi:

1. Nel tuo codice Gleam, importare il modulo `gleam/string`
2. Utilizzare la funzione `to_lower` fornendo una stringa come argomento
3. Salvare il risultato come variabile o utilizzarlo direttamente nel tuo codice

Ecco un esempio di codice e l'output risultante:

```Gleam
let stringa = "HAnNO PenSAto QuEStO PRoGetT0 
modifica = string.to_lower(stringa)
```

Questo produrrà l'output `"hanno pensato questo progetto"`.

## Approfondimento
In Gleam, la funzione `to_lower` utilizza l'algoritmo unicode per convertire una stringa a minuscolo, assicurandosi che funzioni correttamente con tutte le lingue. Inoltre, è anche possibile utilizzare la funzione `to_upper` per convertire una stringa a maiuscolo.

Una cosa importante da notare è che la funzione `to_lower` non modifica la stringa originale, ma ne crea una copia modificata. Se si desidera modificare la stringa originale, è necessario riassegnare la variabile con il risultato della funzione.

## Vedi anche
- Documentazione ufficiale di Gleam sulla funzione `to_lower`
- Tutorial su altre funzionalità di Gleam come la gestione degli errori e le liste