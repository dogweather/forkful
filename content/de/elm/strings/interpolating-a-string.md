---
date: 2024-01-20 17:50:33.702713-07:00
description: "String-Interpolation bedeutet, Variable-Werte direkt in Zeichenketten\
  \ einzuf\xFCgen. Das ist praktisch, um dynamische Texte wie Benutzereingaben oder\u2026"
lastmod: '2024-03-11T00:14:27.684143-06:00'
model: gpt-4-1106-preview
summary: "String-Interpolation bedeutet, Variable-Werte direkt in Zeichenketten einzuf\xFC\
  gen. Das ist praktisch, um dynamische Texte wie Benutzereingaben oder\u2026"
title: Zeichenketten interpolieren
---

{{< edit_this_page >}}

## What & Why?
String-Interpolation bedeutet, Variable-Werte direkt in Zeichenketten einzufügen. Das ist praktisch, um dynamische Texte wie Benutzereingaben oder Berechnungsergebnisse zu erzeugen.

## How to:
Elm hat keine eingebaute String-Interpolation wie du es von JavaScript vielleicht kennst. Stattdessen nutzt man Funktionen wie `String.concat` oder `++` Operator.

```Elm
name = "Welt"
greeting = "Hallo, " ++ name ++ "!"

-- Ausgabe: "Hallo, Welt!"
```

Für komplexere Situationen kannst du `String.join` oder `List.map` mit `++` kombinieren:

```Elm
import List exposing (map, join)

nameList = [ "Alice", "Bob", "Charlie" ]
greetings = map (\name -> "Hallo, " ++ name ++ "!") nameList
joinedGreetings = join " " greetings

-- Ausgabe: "Hallo, Alice! Hallo, Bob! Hallo, Charlie!"
```

## Deep Dive
In Elm ist String-Interpolation nicht direkt eingebaut, weil die Sprache auf Einfachheit und Zuverlässigkeit setzt. In JavaScript kannst du `` `Hallo, ${name}!` `` schreiben, aber in Elm musst du explizite Funktionen verwenden. Früher gab es in anderen Sprachen oft Probleme mit der direkten Interpolation, wie sie etwa zu SQL-Injection führen könnte. Elm's Ansatz vermeidet solche Sicherheitslücken von vornherein.

Alternativen zu `++` könnten eigene Hilfsfunktionen sein, die du für wiederkehrende Muster baust:

```Elm
hello name = "Hallo, " ++ name ++ "!"
goodbye name = "Auf Wiedersehen, " ++ name ++ "!"

-- Verwendung
greeting = hello "Welt"
farewell = goodbye "Freund"

-- Ausgaben
-- greeting: "Hallo, Welt!"
-- farewell: "Auf Wiedersehen, Freund!"
```

## See Also
- Elm Language Documentation: [Elm Lang Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- Gemeinschafts-Beiträge und Tipps: [Elm Discourse](https://discourse.elm-lang.org/)
- Praktische String-Funktionen: [Elm String Extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
