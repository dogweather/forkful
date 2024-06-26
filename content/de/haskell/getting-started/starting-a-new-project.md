---
date: 2024-01-20 18:03:45.041269-07:00
description: 'How to: Hier ist ein minimalistisches Beispiel, um ein Haskell-Projekt
  mit Stack zu starten.'
lastmod: '2024-03-13T22:44:53.932184-06:00'
model: gpt-4-1106-preview
summary: Hier ist ein minimalistisches Beispiel, um ein Haskell-Projekt mit Stack
  zu starten.
title: Einen neuen Projekt starten
weight: 1
---

## How to:
Hier ist ein minimalistisches Beispiel, um ein Haskell-Projekt mit Stack zu starten:

```Haskell
-- Installiere Stack, falls noch nicht geschehen
-- $ curl -sSL https://get.haskellstack.org/ | sh

-- Initialisiere ein neues Projekt namens "meinprojekt"
$ stack new meinprojekt simple

-- Wechsle ins Projektverzeichnis
$ cd meinprojekt

-- Baue das Projekt
$ stack build

-- Führe das Projekt aus
$ stack exec meinprojekt-exe
```

Das `simple` Template erstellt ein grundlegendes Haskell-Projekt mit einem `Main.hs` File. Hier ist dessen Inhalt:

```Haskell
module Main where

main :: IO ()
main = putStrLn "Hallo Welt!"
```

Nach dem Build-Prozess zeigt die Ausführung des Programms:

```Haskell
Hallo Welt!
```

## Deep Dive:
Das Haskell-Tool Stack (seit 2015) ist stark in der Haskell-Community verankert und eine Alternative zu Cabal, dem älteren Build-Tool. Stack bietet reproduzierbare Builds, vereinfachte Paketverwaltung und Integration mit dem Stackage Server, der geprüfte Paket-Sets bereitstellt. Im Gegensatz dazu hat Cabal oft mit "dependency hell" Problemen zu kämpfen gehabt, was Stack durch seine Herangehensweise vermeidet. Stack's Template-System unterstützt Anfänger und Experten gleichermaßen, indem es die Erstellung standardisierter und komplexer Projektstrukturen ermöglicht.

## See Also:
- Stack Dokumentation: https://docs.haskellstack.org/en/stable/README/
- Haskell "Getting Started" Guide: https://www.haskell.org/downloads/
- Stackage Server für Paket-Dokumentation und Versionen: https://www.stackage.org/
- Haskell Paket-Bibliothek (Hackage): http://hackage.haskell.org/
