---
title:                "HTML parsen"
date:                  2024-01-20T15:30:58.368924-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTML-Parser verwandeln HTML-Code in eine Struktur, die Programmiersprachen verstehen. Wir nutzen sie, um die Inhalte und das Design von Webseiten auszulesen oder zu manipulieren.

## How to:

Elm bietet mit `Html.Parser` ein Modul fürs Parsen von HTML. Hier ein einfaches Beispiel:

```Elm
import Html.Parser exposing (..)
import Html.Parser.Attributes exposing (..)

parseHtml : String -> List (Html msg)
parseHtml htmlString =
    parse htmlString
        |> Result.withDefault []

main =
    let
        htmlString = "<p class='text'>Hallo Welt!</p>"
        parsedHtml = parseHtml htmlString
    in
    -- Hier könntest du was mit parsedHtml machen
```

Jetzt hast du eine Elm-Struktur von deinem HTML-String.

## Deep Dive

Elm hat sich bewusst für eine stark typisierte HTML-Parser-Bibliothek entschieden. Das unterscheidet sich von Sprachen wie JavaScript, wo Bibliotheken wie `cheerio` oder die browserintegrierte `DOMParser` API genutzt werden können. Die Typisierung in Elm sorgt für Sicherheit und Zuverlässigkeit, kann aber zu Beginn gewöhnungsbedürftig sein. Die Implementierung basiert auf Funktionen, die HTML als String nehmen und `Result`-Typen zurückgeben, um Fehler zu handhaben.

## See Also

- Elm Html.Parser Dokumentation: [https://package.elm-lang.org/packages/elm/html/latest/Html-Parser](https://package.elm-lang.org/packages/elm/html/latest/Html-Parser)
- Elm Guide zum Umgang mit HTML: [https://guide.elm-lang.org/interop/](https://guide.elm-lang.org/interop/)