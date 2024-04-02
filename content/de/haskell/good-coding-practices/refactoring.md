---
date: 2024-01-26 01:18:24.463871-07:00
description: "Das Refactoring ist der Prozess des Feinjustierens Ihres Codes, ohne\
  \ dessen externes Verhalten zu \xE4ndern. Es geht darum, Ihren Code aufzur\xE4umen\
  \ und zu\u2026"
lastmod: '2024-03-13T22:44:53.939634-06:00'
model: gpt-4-0125-preview
summary: "Das Refactoring ist der Prozess des Feinjustierens Ihres Codes, ohne dessen\
  \ externes Verhalten zu \xE4ndern. Es geht darum, Ihren Code aufzur\xE4umen und\
  \ zu\u2026"
title: Refactoring
weight: 19
---

## Was & Warum?
Das Refactoring ist der Prozess des Feinjustierens Ihres Codes, ohne dessen externes Verhalten zu ändern. Es geht darum, Ihren Code aufzuräumen und zu organisieren, um ihn leichter lesbar, wartbar und erweiterbar zu machen. Es kann auch dabei helfen, Bugs zu beseitigen und die Leistung zu verbessern.

## Wie geht das:
Nehmen wir an, Sie haben einen Haufen Haskell-Code, der sich mehr wiederholt als Ihr Lieblingslied. Hier ist ein schneller Überblick, wie Sie das durch die Verwendung von Funktionen refaktorisieren könnten.

Vor dem Refactoring:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice Kunde Gesamt Artikel = do
  putStrLn $ "Kunde: " ++ Kunde
  putStrLn $ "Gesamt: " ++ show Gesamt
  putStrLn $ "Artikel: " ++ Artikel
```

Nach ein bisschen Refactoring:

```haskell
printDetail :: String -> String -> IO ()
printDetail Beschriftung Wert = putStrLn $ Beschriftung ++ ": " ++ Wert

printInvoice :: String -> Float -> String -> IO ()
printInvoice Kunde Gesamt Artikel = do
  printDetail "Kunde" Kunde
  printDetail "Gesamt" (show Gesamt)
  printDetail "Artikel" Artikel

-- Beispiel Ausgabe:
-- Kunde: Alice
-- Gesamt: $42.00
-- Artikel: Haskell Programmierführer
```

Wie Sie sehen können, vermeiden wir durch Extrahieren des gemeinsamen Musters in eine separate `printDetail`-Funktion Wiederholungen und machen `printInvoice` übersichtlicher und einfacher zu handhaben.

## Tiefer eintauchen
Als Haskell Ende der 80er aufkam, war klar, dass das funktionale Paradigma frischen Wind in die Programmierpraktiken bringen könnte. Schnell vorwärts, und das Refactoring in Haskell ist besonders elegant, dank Funktionen als Bürger erster Klasse und seines starken statischen Typsystems. Sie können refaktorisieren, ohne zu befürchten, dass Sie Ihre App kaputt machen, da der Compiler Sie abgesichert hat.

Alternativen zum manuellen Refactoring könnten die Verwendung von automatisierten Tools beinhalten, obwohl die funktionale Natur und Typsicherheit von Haskell dies manchmal im Vergleich zu anderen Sprachen weniger verbreitet machen. Implementierungsweise ist es wichtig, die Funktionen von Haskell wie höherwertige Funktionen, Reinheit und Unveränderlichkeit zu nutzen, um das Refactoring reibungsloser zu gestalten.

Refactorings wie "Funktion extrahieren", wie gerade gezeigt, sind üblich, aber Sie können auch "Funktion inline setzen", "Variable umbenennen" und "Funktionssignatur ändern" mit Vertrauen durchführen, dank des Typsystems. Die leistungsstarke Typinferenz von Haskell kann manchmal Fehler erwischen, die in anderen Sprachen durchrutschen würden.

## Siehe auch
Für einen tieferen Einblick in das Refactoring in Haskell, schlagen Sie das Buch "Refactoring: Verbesserung der Struktur bestehenden Codes" von Martin Fowler auf, wo die Konzepte universell anwendbar sind. Sehen Sie sich das hlint-Tool für automatisierte Hinweise zur Verbesserung Ihres Haskell-Codes an. Schauen Sie auch im Haskell-Wiki (https://wiki.haskell.org/Refactoring) vorbei für Community-Einsichten und weiterführende Lektüre.
