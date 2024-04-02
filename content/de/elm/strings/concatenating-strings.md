---
date: 2024-01-20 17:34:34.275788-07:00
description: "String-Konkatenierung ist das Zusammenf\xFCgen von zwei oder mehreren\
  \ Textst\xFCcken zu einem einzigen String. Programmierer nutzen das, um dynamische\u2026"
lastmod: '2024-03-13T22:44:53.796811-06:00'
model: gpt-4-1106-preview
summary: "String-Konkatenierung ist das Zusammenf\xFCgen von zwei oder mehreren Textst\xFC\
  cken zu einem einzigen String. Programmierer nutzen das, um dynamische\u2026"
title: "Zeichenketten verkn\xFCpfen"
weight: 3
---

## Was & Warum?
String-Konkatenierung ist das Zusammenfügen von zwei oder mehreren Textstücken zu einem einzigen String. Programmierer nutzen das, um dynamische Textinhalte zu erstellen oder bestehende Informationen zu erweitern.

## Vorgehensweise:
```Elm
import Html exposing (text)

main =
    let
        gruss = "Hallo"
        welt = "Welt!"
        begruessung = gruss ++ " " ++ welt
    in
    text begruessung
```

Ausgabe: "Hallo Welt!"

Du kannst auch mehrere Strings mit der `++` Funktion zusammenhängen:

```Elm
import Html exposing (text)

main =
    text ("Elm " ++ "ist " ++ "toll!")
```

Ausgabe: "Elm ist toll!"

## Vertiefung:
Historisch gesehen haben Programmiersprachen wie C die Konkatenierung mit einfachen Operationen, wie dem `+` Operator, ermöglicht. Elm benutzt stattdessen den `++` Operator für Klarheit und Konsistenz – es ist deutlich, dass es um das Verbinden von Strings geht. Alternativ kann die `String.concat` Funktion für eine Liste von Strings verwendet werden, was nützlich ist, wenn du mit einer größeren Menge an Strings arbeitest. Auf Implementierungsebene optimiert Elm die Konkatenierung, sodass sie auch bei langen Strings effizient ist, was jedoch für die alltägliche Verwendung weniger relevant ist.

## Siehe auch:
- Elm-Dokumentation zur String-Manipulation: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm-Guide zu Textausgaben: https://guide.elm-lang.org/text/
- String.concat Funktion: https://package.elm-lang.org/packages/elm/core/latest/String#concat
