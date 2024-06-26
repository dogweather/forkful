---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:20.190125-07:00
description: 'Come fare: In Haskell, puoi capitalizzare una stringa utilizzando la
  libreria standard senza bisogno di librerie di terze parti.'
lastmod: '2024-03-13T22:44:43.458980-06:00'
model: gpt-4-0125-preview
summary: In Haskell, puoi capitalizzare una stringa utilizzando la libreria standard
  senza bisogno di librerie di terze parti.
title: Capitalizzare una stringa
weight: 2
---

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
