---
date: 2024-01-20 17:34:59.074995-07:00
description: "So geht\u2019s: In Haskell nutzen wir den Operator `(++)`, um Strings\
  \ zu verketten."
lastmod: '2024-03-13T22:44:53.923007-06:00'
model: gpt-4-1106-preview
summary: In Haskell nutzen wir den Operator `(++)`, um Strings zu verketten.
title: "Zeichenketten verkn\xFCpfen"
weight: 3
---

## So geht’s:
In Haskell nutzen wir den Operator `(++)`, um Strings zu verketten.
```Haskell
main :: IO ()
main = do
    let begruessung = "Hallo, "
        name = "Welt!"
    putStrLn (begruessung ++ name)  -- Ausgabe: Hallo, Welt!
```
`putStrLn` schreibt den resultierenden String auf die Konsole.

## Deep Dive
Struktur und Effizienz sind wichtig bei der Verkettung. Historisch gesehen kamen Listen zum Einsatz – Strings in Haskell sind Listen von Char. Listenverkettung `(++)` ist einfach, aber nicht immer effizient, besonders bei langen Strings. Alternativen? `Data.Text` für große Textmengen und `Builder` aus dem `Data.Text.Lazy.Builder` für eine effizientere Verkettung durch Nutzung eines Zwischenpuffers.

Haskell’s Garbage Collector räumt auf, dennoch können viele Verkettungen zu viel unnötigen Speicherplatz verbrauchen. Beachte also den Kontext: 'Kurz und oft' oder 'lang und selten' machen einen großen Unterschied!

## See Also
- [Haskell Documentation for `Data.Text`](https://hackage.haskell.org/package/text)
- [Performance considerations with Strings](https://wiki.haskell.org/Performance/Strings)
- [Learn You a Haskell for Great Good! on Strings](http://learnyouahaskell.com/starting-out#strings)
