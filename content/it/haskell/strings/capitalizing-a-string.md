---
aliases:
- /it/haskell/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:20.190125-07:00
description: "Trasformare la prima lettera di una stringa in maiuscolo, garantendo\
  \ che il resto delle lettere rimanga in minuscolo, \xE8 ci\xF2 che viene definito\u2026"
lastmod: 2024-02-18 23:08:55.913608
model: gpt-4-0125-preview
summary: "Trasformare la prima lettera di una stringa in maiuscolo, garantendo che\
  \ il resto delle lettere rimanga in minuscolo, \xE8 ci\xF2 che viene definito\u2026"
title: Capitalizzare una stringa
---

{{< edit_this_page >}}

## Cosa & Perché?
Trasformare la prima lettera di una stringa in maiuscolo, garantendo che il resto delle lettere rimanga in minuscolo, è ciò che viene definito capitalizzazione di una stringa. I programmatori eseguono questa operazione per formattare gli output, aderire alla correttezza grammaticale nei testi o migliorare la leggibilità dei dati generati.

## Come fare:
In Haskell, puoi capitalizzare una stringa utilizzando la libreria standard senza bisogno di librerie di terze parti.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Utilizzo di esempio:
main = putStrLn $ capitalize "hello world"
```

Output:
```
Hello world
```

Per scenari più complessi o per una maggiore facilità d'uso, potresti voler utilizzare una libreria di terze parti come `text`, popolare per la manipolazione efficiente delle stringhe in Haskell.

Per prima cosa, devi aggiungere `text` alle dipendenze del tuo progetto. Poi, puoi utilizzare le sue funzioni per capitalizzare una stringa nel modo seguente:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Utilizzo di esempio con la libreria text:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Output:
```
Hello world
```

Entrambi questi esempi dimostrano modi semplici ma efficaci per capitalizzare una stringa in Haskell, con o senza librerie di terze parti.
