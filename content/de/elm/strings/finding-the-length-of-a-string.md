---
date: 2024-01-20 17:47:16.838925-07:00
description: "\"Was & Warum?\" - Die L\xE4nge eines Strings festzustellen bedeutet,\
  \ die Anzahl der Zeichen zu ermitteln. Programmierer machen das oft, um die Eingabe\
  \ zu\u2026"
lastmod: 2024-02-19 22:05:12.721202
model: gpt-4-1106-preview
summary: "\"Was & Warum?\" - Die L\xE4nge eines Strings festzustellen bedeutet, die\
  \ Anzahl der Zeichen zu ermitteln. Programmierer machen das oft, um die Eingabe\
  \ zu\u2026"
title: "Ermittlung der Zeichenkettenl\xE4nge"
---

{{< edit_this_page >}}

## What & Why?
"Was & Warum?" - Die Länge eines Strings festzustellen bedeutet, die Anzahl der Zeichen zu ermitteln. Programmierer machen das oft, um die Eingabe zu validieren, Daten zu verarbeiten oder das Layout zu steuern.

## How to:
"So geht's" - Hier ein paar Elm-Beispiele, wie du die Länge eines Strings herausfindest:

```Elm
module Main exposing (..)
import Html exposing (text)

main =
    text (String.fromInt (String.length "Hallo Welt!"))

-- Ausgabe: "11"
```
Ganz einfach. Verwende `String.length`, um die Zeichenanzahl zu bekommen und `String.fromInt` um das Ergebnis in einen String zu konvertieren.

## Deep Dive
"Tiefer eintauchen" - Elm's `String.length` Funktion gibt dir die Länge eines Strings in konstanter Zeit – das ist effizient. Historisch gesehen, waren Operationen mit Strings oft langsamer. In einigen Sprachen, insbesondere in früheren Versionen, kann es unterschiedliche Herangehensweisen geben, abhängig von der String-Implementierung.

In Elm wird ein String intern als UTF-16 codiert, also können Zeichen, die außerhalb des Basis-Mehrsprachigen Plans (BMP) liegen, als zwei "code units" gezählt werden. Für die meisten Anwendungen ist dies jedoch irrelevant.

Es gibt Alternativen, wie `List.length (String.toList "DeinString")`, aber das ist nicht nötig und weniger performant, da `String.length` optimiert ist.

## See Also
"Siehe auch" - Schau dir diese Ressourcen für weitere Informationen an:

- Elm String Docs: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
