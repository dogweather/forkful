---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ist das Einsetzen von Werten in einen String. Programmierer machen das für die bessere Lesbarkeit und einfachere Codierung.

## So geht's:
In Elm kann die Funktion `String.fromInt` verwendet werden, um Zahlen in Strings zu konvertieren. Hier ist ein Beispiel:

```Elm
import Html exposing (Html, text)
import String

main : Html msg
main =
    let 
        answer = 42
        output = "Die Antwort ist " ++ String.fromInt answer
    in
        text output
```

Laufausgabe wäre:

```
Die Antwort ist 42
```

## Vertiefung
Historisch gesehen wurde String-Interpolation in vielen Sprachen verwendet. In Elm wird String-Konkatenation statt String-Interpolation verwendet, da Elm auf Funktionen statt auf Syntax-Erweiterungen setzt. Alternativen wie `String.concat` können auch verwendet werden, sie sind jedoch weniger lesbar. Die Implementierung ist einfach und direkt – Werte werden einfach zu Strings konvertiert und dann verbunden.

## Siehe auch
- [Elm String Library](https://package.elm-lang.org/packages/elm/core/latest/String)
- [String fromInt documentation](https://package.elm-lang.org/packages/elm/core/latest/String#fromInt)