---
title:                "Capitalizzare una stringa"
html_title:           "Haskell: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. I programmatori lo fanno di solito per migliorare la leggibilità di un testo.

## Come si fa:

Ecco un esempio di come capitalizzare una stringa in Haskell.

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

main = print (capitalize "ciao mondo")
```

L'output sarà:

```Haskell
"Ciao mondo"
```

## Approfondimento

In termini di contesto storico, Haskell è stato uno dei primi linguaggi di programmazione a supportare funzioni di alto livello come `toUpper` per manipolare stringhe in maniera semplice e diretta.

Un'alternativa alla funzione `capitalize` che abbiamo definito sopra è utilizzare la funzione `map` di Haskell combinata con `toUpper`, così da capitalizzare tutti i caratteri in una stringa, non solo il primo.

```Haskell
allCaps :: String -> String
allCaps = map toUpper

main = print (allCaps "ciao mondo")
```

L'output sarà: 

```Haskell
"CIAO MONDO"
```

Per quel che riguarda l'implementazione, la funzione `toUpper` fa parte del modulo `Data.Char` in Haskell e utilizza il codice ASCII per convertire le lettere minuscole in maiuscole.

## Da Vedere Anche

Per ulteriori informazioni su come lavorare con le stringhe in Haskell, consulta le risorse seguenti:

3. [Data.Char Module Documentation](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html)