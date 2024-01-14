---
title:                "Elm: Lesen von Befehlszeilenargumenten"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein wichtiger Teil der Programmierung in Elm. Es ermöglicht es dir, auf die Eingaben deiner Benutzer zu reagieren und interaktive Programme zu erstellen. In diesem Blog-Beitrag werden wir uns genauer mit diesem Thema befassen und lernen, wie man Befehlszeilenargumente liest.

## Wie geht man vor

Um Befehlszeilenargumente zu lesen, müssen wir zuerst ein Modul importieren, das uns hilft, auf die Argumente zuzugreifen:

```Elm
import Basics exposing (..)
import Platform.Cmd exposing (args)
```

Als nächstes können wir die Funktion `elmApp` verwenden, um die Argumente als Liste von Zeichenketten zu erhalten. Diese Funktion erwartet, dass wir ihr ein Mapping übergeben, welches die Argumente verarbeitet. Hier ist ein einfaches Beispiel:

```Elm
main =
  elmApp processArgs

processArgs arguments =
  case arguments of
    [] -> Text.color Text.red <| Text.fromString "Keine Argumente angegeben!"
    _ -> Text.fromString "Argumente erhalten!"
```

In diesem Beispiel verwenden wir das `elmApp`-Mapping, um auf alle Argumente zuzugreifen und sie dann mit einem einfachen Musterabgleich zu verarbeiten. Wenn keine Argumente angegeben werden, wird eine Meldung in roter Farbe angezeigt, ansonsten wird einfach bestätigt, dass die Argumente erhalten wurden.

## Tiefer eintauchen

Das `args`-Modul bietet mehrere Funktionen, die uns dabei helfen, die Befehlszeilenargumente weiter zu verarbeiten. Beispielsweise können wir die Funktion `getAt` verwenden, um ein bestimmtes Argument auszuwählen oder die Funktion `length` verwenden, um die Anzahl der Argumente zu ermitteln.

```Elm
main =
  elmApp processArgs

processArgs arguments =
  if length arguments > 2 then
    let 
      firstArg = getAt 0 arguments
      secondArg = getAt 1 arguments
      thirdArg = getAt 2 arguments
    in
      Text.concat [ Text.fromString "Die ersten drei Argumente sind: "
                  , firstArg
                  , Text.fromString ", "
                  , secondArg
                  , Text.fromString " und "
                  , thirdArg
                  ]
  else
    Text.fromString "Nicht genug Argumente angegeben!"
```

In diesem Beispiel nutzen wir `length` und `getAt`, um die ersten drei Argumente aus der Liste auszuwählen und sie dann in einer zusammengefügten Zeichenkette anzuzeigen.

## Siehe auch

- [Dokumentation zum Platform.Cmd-Modul](https://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Cmd)
- [Codebeispiel zum Lesen von Befehlszeilenargumenten](https://github.com/elm-lang/core/blob/master/tests/Platform/Platform/Platform/Command.elm)