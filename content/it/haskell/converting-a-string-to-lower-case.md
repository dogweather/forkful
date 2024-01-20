---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Convertire una stringa in minuscolo in programmazione significa trasformare tutti i caratteri alfabetici di una stringa in minuscolo. I programmatori lo fanno per uniformare i dati e semplificare le operazioni di confronto e analisi.

## Come si fa:

Haskell offre una funzione integrata, `toLower`, che può essere utilizzata per convertire un singolo carattere in minuscolo. Per convertire una stringa completa, usiamo `map` per applicare `toLower` a ogni carattere della stringa.

```Haskell
import Data.Char (toLower)

stringaMinuscolo :: String -> String
stringaMinuscolo = map toLower
```

Esempio di utilizzo:

```Haskell 
main = putStrLn $ stringaMinuscolo "CIAO MONDO"
```

Risultato:
```
ciao mondo
```

## Approfondimento

La funzione `toLower` in Haskell implementa lo standard Unicode per la conversione in minuscolo, e non si limita ai soli caratteri ASCII. Questo consente di gestire varie lingue e insiemi di caratteri.

Le alternative sono la scrittura di una funzione personalizzata, che può effettuare trasformazioni specifiche (come ignorare caratteri non alfabetici), o l'uso di librerie esterne che offrono funzionalità più sofisticate, come il supporto per le regole di composizione delle varie lingue.

L'implementazione di `toLower` si basa sulla tabella Unicode: ogni carattere viene mappato al suo equivalente minuscolo, se esiste.

## Guarda Anche

Per ulteriori informazioni sulla programmazione in Haskell e le stringhe, consulta le seguenti risorse:

- Real World Haskell, capitolo 8: [http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)
- Haskell Wiki, su stringhe: [https://wiki.haskell.org/String](https://wiki.haskell.org/String)
- Documentazione Haskell su Data.Char: [https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)