---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Was und Warum?
Beim Parsen von HTML wird eine HTML-Datei in ein Format umgewandelt, das von Programmiersprachen verarbeitet werden kann. Programmierer tun dies, um Informationen aus der Struktur und den Inhalten von Webseiten zu extrahieren.

## So funktionierts:
Hier ein typischer Elm-Code zum Parsen von HTML:

```Elm
import Html exposing (Html, div, text)
import Html.Parser exposing (node, text, decode, run)

parseNode : String -> Html msg
parseNode htmlStr = 
  case run decode htmlStr of
    Ok html -> html
    Err _   -> div [] [ text "Parsing error." ]

decode : Parser (Html msg)
decode = 
  node "div" [] [ text ]
```

Wenn Sie diese Codezeile ausführen `parseNode "<div>Hello World</div>"`, wird die Ausgabe `Hello World` sein.

## Deep Dive
Historisch gesehen wurde HTML-Parsing ursprünglich für Web-Crawling und Datenextraktion eingesetzt. Alternativ könnten Sie auch reguläre Ausdrücke verwenden, aber das ist meistens schwieriger und fehleranfällig. In Elm erledigen die Pakete `Html.Parser` und `Html.Parser.run` die meiste Arbeit hinter den Kulissen.

## Siehe auch
- Elm Docs zu [HTML Parser](https://package.elm-lang.org/packages/eeue56/elm-html-parser/latest/)
- Ein einfacher [HTML Parser Tutorial](https://elmprogramming.com/decoding-html.html) auf elmprogramming.com
- Weitere Informationen zur [Html.Parser.run Funktion](https://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html-Parser#run)