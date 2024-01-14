---
title:                "Elm: Estrazione di sottostringhe"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Con l'uso sempre più diffuso della programmazione funzionale, l'estrazione di sottostringhe è diventata una pratica comune per gestire i dati. Con Elm, questa operazione è particolarmente facile e veloce da eseguire.

## Come fare

Per estrarre una sottostringa in Elm, è necessario utilizzare la funzione `slice` presente nel modulo `String` di Elm. Questa funzione accetta tre argomenti: l'indice di inizio, l'indice di fine e la stringa da cui estrarre la sottostringa. Ad esempio:

```Elm
import String exposing (slice)

myString = "Buon compleanno a tutti!"

substring = slice 5 15 myString

```

Il codice sopra estrae la sottostringa "compleanno" dalla nostra stringa `myString`. L'indice di inizio 5 indica il carattere "c", mentre l'indice di fine 15 indica il carattere "o" di "compleanno". Il risultato viene assegnato alla variabile `substring` e può essere utilizzato successivamente nel nostro codice.

## Approfondimenti

Ci sono alcune cose da tenere a mente quando si lavora con la funzione `slice`:

- Gli indici di inizio e fine inclusi nella funzione `slice` sono basati su zero, il che significa che il primo carattere ha indice 0 e non 1. Questo è un concetto comune nella programmazione e dovrebbe essere preso in considerazione quando si utilizza `slice`.
- Se l'argomento di fine è maggiore o uguale alla lunghezza della stringa, la sottostringa estratta sarà fino alla fine della stringa.
- Se l'argomento di inizio è negativo, viene contanto dall'ultimo carattere della stringa. Ad esempio, un indice di inizio di -1 corrisponde all'ultimo carattere della stringa, -2 al penultimo e così via.
- Se l'argomento di fine è negativo, viene contanto dalla fine della stringa. Ad esempio, un indice di fine di -1 corrisponde all'ultimo carattere della stringa, -2 al penultimo e così via.

## Vedi anche

- Documentazione della funzione `slice` nel modulo String di Elm: https://package.elm-lang.org/packages/elm/core/latest/String#slice
- Un articolo utile su come utilizzare la funzione `slice` per manipolare stringhe in Elm: https://rundis.github.io/blog/elm/slice-and-concat