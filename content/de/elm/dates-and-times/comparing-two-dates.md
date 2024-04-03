---
date: 2024-01-20 17:32:54.544929-07:00
description: "Das Vergleichen von zwei Daten entscheidet, welches Datum fr\xFCher\
  \ oder sp\xE4ter ist oder ob sie gleich sind. Programmierer nutzen das, um Zeitabl\xE4\
  ufe zu\u2026"
lastmod: '2024-03-13T22:44:53.817717-06:00'
model: gpt-4-1106-preview
summary: "Das Vergleichen von zwei Daten entscheidet, welches Datum fr\xFCher oder\
  \ sp\xE4ter ist oder ob sie gleich sind."
title: Vergleich von zwei Daten
weight: 27
---

## Anleitung:
Elm macht das Vergleichen von Daten ziemlich schlicht. Hier ein paar Code-Beispiele:

```Elm
import Time exposing (Posix)
import Basics exposing (compare)

-- Zwei Posix-Daten zum Vergleichen vorbereiten
posix1 : Posix
posix1 =
    Time.millisToPosix 1580515200000  -- 1. Februar 2020
  
posix2 : Posix
posix2 =
    Time.millisToPosix 1609459200000  -- 1. Januar 2021

-- Daten vergleichen
compareDates : Posix -> Posix -> Basics.Order
compareDates date1 date2 =
    compare date1 date2

-- Vergleich ausführen und Ergebnis darstellen
main =
    toString (compareDates posix1 posix2)
    -- "GT" bedeutet, dass das erste Datum (posix1) größer (später) ist als das zweite Datum (posix2)
```

## Tiefgang:
Historisch gesehen, war das Vergleichen von Daten schon immer ein zentraler Bestandteil der Programmierung, weil es grundlegend für die Verwaltung von Ereignissen im Zeitverlauf ist. In Elm, das seit 2012 existiert, geschieht der Vergleich durch Nutzung der Posix-Zeit, einer Zählung von Millisekunden seit dem 1. Januar 1970, auch bekannt als Unix-Zeit. Alternativ könnten auch Bibliotheken wie `elm-time` verwendet werden, die komfortablere Funktionen zum Umgang mit Daten bereitstellen. Die Implementierung in Elm selbst ist recht geradlinig und nutzt die puren Funktionen der Programmiersprache, um Vorhersehbarkeit und Zuverlässigkeit beim Vergleich von Daten zu gewährleisten.

## Siehe auch:
- Elm Time Library: [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
- Elm Language Guide (Zeit und Datum): [https://guide.elm-lang.org/effects/time.html](https://guide.elm-lang.org/effects/time.html)
- Unix Zeit Konverter: [https://www.unixtimestamp.com/](https://www.unixtimestamp.com/)
