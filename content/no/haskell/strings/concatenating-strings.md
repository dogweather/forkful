---
date: 2024-01-20 17:35:00.273360-07:00
description: 'Hvordan: I Haskell, bruker vi `(++)` operatoren eller `concat` funksjonen.
  Her er litt kode.'
lastmod: '2024-03-13T22:44:40.832902-06:00'
model: gpt-4-1106-preview
summary: I Haskell, bruker vi `(++)` operatoren eller `concat` funksjonen.
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Hvordan:
I Haskell, bruker vi `(++)` operatoren eller `concat` funksjonen. Her er litt kode:

```Haskell
-- Bruker (++)
hilsen :: String
hilsen = "Hei, " ++ "verden!"

-- Output: "Hei, verden!"

-- Med concat
navnListe :: [String]
navnListe = concat ["Norden", " og ", "Sør"]

-- Output: "Norden og Sør"
```

## Dypdykk
I Haskell, som er en funksjonell programmeringsspråk, er strengsammenslåing en grunnleggende operasjon. Funksjonaliteten har vært en del av Haskell siden starten (på 1990-tallet), inspirert av lignende konsepter i tidligere funksjonelle språk som Lisp. Det finnes alternativer til `(++)` som `concat`, `intercalate` og `foldr`, som gir ulike nyanser for mer kompliserte behov. Internt, strenger er lister av tegn (`[Char]`), og sammenkjeding blir en liste-operasjon.

## Se Også
- [Learn You a Haskell for Great Good! on Strings](http://learnyouahaskell.com/starting-out#strings)
