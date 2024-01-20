---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La ricerca della lunghezza di una stringa è un'azione comune nella programmazione, nota come "ottenere la lunghezza di una stringa". Questa operazione è fondamentale per vari scenari, come la manipolazione di testo, il conteggio dei caratteri e la validazione dell'input utente.

## Come Fare

Ecco un breve esempio su come ottenere la lunghezza di una stringa in Gleam:

```gleam
import gleam/string

let lunghezza = string.length("Ciao, Mondo!")
```
Quando esegui questi codice, otterrai la lunghezza del testo "Ciao, Mondo!", che è 12.

## Approfondimento

La funzione `string.length` in Gleam, come molte altre funzioni di conteggio delle stringhe nel mondo della programmazione, risale all'era del linguaggio C. In quei tempi, le stringhe erano gestite come array di caratteri, pertanto trovare la lunghezza di una stringa significava semplicemente iterare gli elementi dell'array fino a raggiungere un carattere nullo.

Tra le alternative, alcune funzioni di conteggio delle stringhe tengono conto dei caratteri multibyte, il che è importante se il tuo programma sta gestendo stringhe Unicode. Tuttavia, `string.length` in Gleam fornisce la lunghezza in termini di codepoints Unicode, quindi non devi preoccuparti di questo problema.

Quando si parla di implementazione, è importante notare che la funzione `string.length` in Gleam è altamente ottimizzata. Sotto il cofano, non esegue un ciclo attraverso ogni carattere, ma piuttosto utilizza le proprietà interne del linguaggio Erlang sottostante per calcolare la lunghezza di una stringa in tempo costante, il che rende questa funzione molto efficiente.

## Vedi Anche

Per ulteriori informazioni sulla gestione delle stringhe in Gleam, vedere le seguenti risorse:

1. [Gleam String Module Documentation](https://hexdocs.pm/gleam_stdlib/gleam/string.html)