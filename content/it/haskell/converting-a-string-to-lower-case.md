---
title:                "Convertire una stringa in minuscolo"
html_title:           "Haskell: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Sai quel momento in cui devi confrontare due stringhe e vuoi essere sicuro che la loro differenza non sia solo una questione di lettere maiuscole e minuscole? Ecco, qui è dove la conversione di una stringa in caratteri minuscoli può essere di grande aiuto.

## Come fare

È molto semplice convertire una stringa in caratteri minuscoli in Haskell. Basta usare la funzione `toLower` del modulo `Data.Char`.

```Haskell
import Data.Char

-- Esempio 1
toLower "CIAO" -> "ciao"

-- Esempio 2
toLower "HeLLo WoRlD" -> "hello world"
```

## Approfondimento

Per coloro che sono interessati a sapere come funziona la conversione delle stringhe in Haskell, esiste un concetto chiamato "ricorsione" che è utilizzato per iterare attraverso una stringa e applicare la funzione `toLower` a ogni singolo carattere.

La funzione `toLower` è definita nel seguente modo:

```Haskell
toLower :: Char -> Char

-- Se il carattere è già minuscolo, viene restituito così com'è
toLower c | c >= 'a' && c <= 'z' = c
          -- Se è maiuscolo, viene sottratto 32 (la differenza tra i codici ASCII di "A" e "a")
          | c >= 'A' && c <= 'Z' = chr(ord c - 32)
          | otherwise = c -- Se è un altro tipo di carattere, viene restituito così com'è
```

Inoltre, esistono anche altre funzioni utili per la conversione delle stringhe, come `toUpper` e `toTitle`, che trasformano rispettivamente tutti i caratteri in maiuscolo o solo la prima lettera di ogni parola.

## Vedi anche

- [Funzioni di conversione delle stringhe in Haskell](https://www.haskell.org/hoogle/?hoogle=String+-%3E+String)
- [Modulo `Data.Char` su Hackage](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)