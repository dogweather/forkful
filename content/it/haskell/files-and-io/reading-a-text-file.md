---
date: 2024-01-20 17:54:36.829276-07:00
description: "Leggere un file di testo permette ai programmi di processare dati salvati\
  \ su disco. Programmatori lo fanno per analizzare, modificare o semplicemente\u2026"
lastmod: '2024-03-13T22:44:43.492841-06:00'
model: gpt-4-1106-preview
summary: "Leggere un file di testo permette ai programmi di processare dati salvati\
  \ su disco. Programmatori lo fanno per analizzare, modificare o semplicemente\u2026"
title: Lettura di un file di testo
---

{{< edit_this_page >}}

## What & Why?
Leggere un file di testo permette ai programmi di processare dati salvati su disco. Programmatori lo fanno per analizzare, modificare o semplicemente visualizzare il contenuto del file.

## How to:

Leggere tutto il contenuto di un file:

```Haskell
import System.IO

main :: IO ()
main = do
    content <- readFile "esempio.txt"
    putStrLn content
```

Output:
```
Questo è il contenuto del file di testo.
```

Leggere riga per riga:

```Haskell
import System.IO

stampaRighe :: Handle -> IO ()
stampaRighe handle = do
    eof <- hIsEOF handle
    if eof
        then return ()
        else do
            riga <- hGetLine handle
            putStrLn riga
            stampaRighe handle

main :: IO ()
main = do
    handle <- openFile "esempio.txt" ReadMode
    stampaRighe handle
    hClose handle
```

## Deep Dive

La lettura dei file di testo in Haskell è stata storicamente influenzata dal desiderio del linguaggio di trattare le operazioni di input/output (IO) in un modo funzionale pur mantenendo la purezza. Per questo motivo, le operazioni IO sono segregate nel tipo `IO`.

Esistono alternative alla funzione `readFile`, come `readLn` o pacchetti di terze parti come `text` e `bytestring` che offrono performance migliorate o ulteriori funzionalità.

In dettaglio, `readFile` è una funzione non bloccante, che significa che viene restituito immediatamente un "handle" per i dati, e i dati vengono letti effettivamente quando se ne ha bisogno. Questo può essere utile per la gestione di grandi quantità di dati senza sovraccaricare la memoria.

## See Also

- [Haskell Docs - System.IO](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html)
- Pacchetto [`text`](https://hackage.haskell.org/package/text)
- Pacchetto [`bytestring`](https://hackage.haskell.org/package/bytestring)
- Tutorial su [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/input-and-output)
