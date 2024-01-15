---
title:                "HTML analysieren"
html_title:           "Elm: HTML analysieren"
simple_title:         "HTML analysieren"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Warum
Parsing von HTML ist ein wichtiger Teil der Entwicklung von Webanwendungen, da es ermöglicht, strukturierte Daten aus dem HTML Code zu extrahieren und in ein maschinenlesbares Format umzuwandeln. Dadurch können wir die Daten einfacher verarbeiten und benutzen.

## Wie
Das Parsen von HTML in Elm ist relativ einfach und wird durch die Elm Html Bibliothek unterstützt. Es gibt verschiedene Funktionen, die in Kombination verwendet werden können, um HTML in Elm zu parsen.

Ein Beispiel für das Parsen von HTML sieht wie folgt aus:

```Elm
import Html exposing (..)
import Html.Parser exposing (Attribute(..), Element(..), Parser)

parseHtml : String -> Maybe Html
parseHtml str =
    case Html.Parser.parseHtml str of
        Ok element ->
            Just element

        Err _ ->
            Nothing
```

Die Funktion `parseHtml` nimmt einen String als Eingabe und gibt entweder ein `Just` mit dem geparsten HTML Element oder ein `Nothing` zurück, wenn ein Fehler auftritt.

Einmal geparst, können wir das HTML Element in Elm verwenden, um die Daten zu extrahieren oder zu manipulieren. Zum Beispiel können wir alle Links auf der Seite finden und sie in eine Liste zusammenstellen:

```Elm
import Html exposing (..)
import Html.Parser exposing (Attribute(..), Element(..), Parser)

getPageLinks : Html -> List String
getPageLinks html =
    let
        links = Html.xpath "//a/@href" html
        links' = List.map (\(Attribute _ value) -> value) links
    in
        links'
```

Die Funktion `getPageLinks` nimmt das geparste HTML Element als Eingabe und verwendet die `xpath` Funktion aus der `Html` Bibliothek, um alle `href` Attribute der `a` Elemente auf der Seite zu finden. Diese Attribute werden dann in eine Liste von Strings umgewandelt und zurückgegeben.

## Deep Dive
Das Parsen von HTML kann komplex werden, wenn wir uns mit unterschiedlichen Syntaxen und Sonderfällen auseinandersetzen müssen. Die Elm Html Bibliothek bietet jedoch verschiedene Funktionen, die uns dabei helfen, damit umzugehen.

Zum Beispiel können wir mit der `Parser` Bibliothek Attribute mit unterschiedlichen Werten parsen, indem wir die `Attribute` Union Type verwenden. Außerdem können wir Mustererkennung in Kombination mit der `Element` Union Type verwenden, um spezifische Elemente aus dem HTML zu extrahieren und zu verarbeiten.

Es ist auch wichtig zu beachten, dass das Parsen von HTML nicht immer die beste Option ist. In manchen Fällen kann es einfacher sein, die HTML Struktur direkt in Elm zu codieren, anstatt sie zu parsen.

## Siehe Auch
- [Offizielle Elm Html Dokumentation](https://package.elm-lang.org/packages/elm/html/latest/)
- [Elm Html Parser Paket](https://package.elm-lang.org/packages/elm/parser/latest/)