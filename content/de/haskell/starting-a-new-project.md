---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Ein neues Haskell-Projekt starten: Eine Anleitung

## Was und Warum?

Ein neues Projekt zu starten bedeutet, eine leere Leinwand zu haben, auf der Sie Ihren Code schaffen. Programmierer machen das, um ihre Ideen in die Realität umzusetzen und Lösungen für Probleme zu entwickeln.

## Wie es Funktioniert:

In Haskell starten wir ein neues Projekt normalerweise mithilfe von Stack. Hier ist ein Beispiel:

```Haskell
-- Installation Stack
$ curl -sSL https://get.haskellstack.org/ | sh

-- Neues Projekt erstellen
$ stack new myproject
```
Dadurch entsteht eine neue Projektstruktur, und Sie können Ihren Code in der `Main.hs`-Datei in `src` schreiben.

```Haskell
-- src/Main.hs

module Main where

main :: IO ()
main = putStrLn "Hallo, Welt!"
```

Führen Sie dann Ihren Code mit Stack aus:

```Haskell
$ stack build
$ stack exec myproject
Hallo, Welt!
```

## Tiefere Einblicke

Historisch gesehen gibt es in Haskell verschiedene Wege, neue Projekte zu starten, wie z.B. Cabal oder reines GHC. Stack ist jedoch aufgrund seiner Einfachheit und Zuverlässigkeit eine populäre Wahl geworden.

Abgesehen von Stack bietet auch Cabal eine ausgezeichnete Möglichkeit, neue Haskell-Projekte zu starten, insbesondere wenn Sie eine Feinkontrolle über die verwendeten Bibliotheken und GHC-Versionen benötigen.

Bei der Implementierung eines neuen Projekts in Haskell ist es wichtig, auf Modulstruktur und Programmarchitektur zu achten. Haskell ist eine sehr ausdrucksstarke Sprache, und es ist leicht, sich in komplexen Abstraktionen zu verlieren.

## Siehe Auch

- [Haskell Stack Documentation](https://docs.haskellstack.org/en/stable/README)
- [Introduction to Cabal](https://www.haskell.org/cabal/users-guide/intro.html)