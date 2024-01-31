---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster, um Text nach spezifischen Regeln zu durchsuchen und zu manipulieren. Sie sparen Zeit und Codezeilen, wenn es um komplexe Textoperationen geht.

## How to:
Elm hat standardmäßig keine eingebauten Funktionen für reguläre Ausdrücke, daher benötigst du das Paket `elm/regex`. Hier ist ein Beispiel, wie man es benutzt:

```Elm
import Regex

findUrls : String -> List String
findUrls text =
    let
        regex = Regex.fromString "https?://[\\w-_.]+"
    in
    case regex of
        Nothing ->
            []

        Just re ->
            Regex.find re text
                |> List.map .match
```

Ausführen von `findUrls "Check out this website: https://example.com and this one: http://elm-lang.org"` gibt `[ "https://example.com", "http://elm-lang.org" ]` zurück.

## Deep Dive
Reguläre Ausdrücke kamen in den 1950ern auf und sind seitdem in fast allen Programmiersprachen verfügbar. Alternativen zu regulären Ausdrücken sind String-Parser oder spezialisierte Such- und Ersetzungs-Funktionen, die jedoch oft weniger leistungsfähig sind. Wenn man reguläre Ausdrücke in Elm einsetzt, laufen sie auf JavaScripts RegEx-Engine, da Elm zu JavaScript kompiliert wird.

## See Also:
- Elm Regex Paket: https://package.elm-lang.org/packages/elm/regex/latest/
- Regex101 zum Testen von regulären Ausdrücken: https://regex101.com/
- Elm Guide für String-Parsing: https://guide.elm-lang.org/patterns/string_parsing.html
