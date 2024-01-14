---
title:    "Elm: Lesen von Befehlszeilenargumenten"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Befehlszeilenargumenten ist ein wichtiger Teil der Entwicklung von Elm-Anwendungen. Es ermöglicht, dass Programme flexibler und interaktiver gestaltet werden können. In diesem Blogpost werde ich zeigen, wie man Befehlszeilenargumente in Elm liest und verwaltet.

# Wie geht's?

Um Befehlszeilenargumente in Elm zu lesen, müssen wir zunächst das Modul `Platform.Cmd` importieren. Anschließend können wir die Funktion `Cmdline.programWithFlags` verwenden, um das Hauptmodul unserer Anwendung zu erstellen. Diese Funktion erwartet zwei Argumente: eine Funktion, die die Befehlszeilenargumente verarbeitet, und eine Funktion, die das eigentliche Programm initialisiert.

Ein Beispiel für eine einfache Elm-Anwendung, die Befehlszeilenargumente liest, könnte wie folgt aussehen:

```Elm
import Platform.Cmd exposing (..)

type alias Args =
  { name : String
  , age : Int
  }

init : (Args -> Cmd msg) -> Cmd msg
init handleArgs =
  Cmdline.programWithFlags
    (handleArgs << toArgs)
    []

toArgs : List String -> Args
toArgs args =
  case args of
    [name, age] ->
      { name = name
      , age = String.toInt age |> Result.withDefault 0
      }
    _ ->
      { name = "Unknown"
      , age = 0
      }

handleArgs : Args -> Cmd msg
handleArgs args =
  Cmd.none

main : Program () () Args
main =
  init handleArgs
```

Wenn wir diese Anwendung mit den Befehlszeilenargumenten "Max 25" aufrufen, erhalten wir als Output folgendes:

```
{ name = "Max", age = 25 }
```

# Tiefentauchen

Es ist auch möglich, Befehlszeilenargumente während der Ausführung des Programms zu ändern. Dazu können wir die Funktion `Platform.Cmd.send` verwenden, um Befehle an den `update`-Funktion unseres Programms zu senden. Ein Beispiel könnte folgendermaßen aussehen:

```Elm
-- restliche Code aus vorherigem Beispiel

type Msg
  = UpdateName String
  | UpdateAge Int

update : Msg -> Args -> ( Args, Cmd Msg )
update msg args =
  case msg of
    UpdateName newName ->
      ( { args | name = newName }, Cmd.none )
    UpdateAge newAge ->
      ( { args | age = newAge }, Cmd.none )

view : Args -> Html Msg
view args =
  div []
    [ input
        [ type_ "text"
        , value args.name
        , onInput (UpdateName << targetValue)  -- Verarbeitung von Benutzereingaben
        ] []
    , input
        [ type_ "number"
        , value (String.fromInt args.age)
        , onInput (UpdateAge << parseInt)       -- Verarbeitung von Benutzereingaben
        ] []
    ]

main : Program () Model Args
main =
  init handleArgs

-- restliche hilfreiche Funktionen aus dem vorherigen Beispiel
```

Nach jedem Eingeben eines Werts in eines der Input-Felder wird die `update`-Funktion mit dem entsprechenden `Msg`-Wert aufgerufen, der dann das Befehlszeilenargument entsprechend aktualisiert.

# Siehe auch

- Offizielle Elm-Dokumentation zu `Platform.Cmd`: https://guide.elm-lang.org/interop/cmd.html
- Elm-Befehlszeilenargument-Parser: https://package.elm-lang.org/packages/elm-community/parser/latest/
- "6 Gründe, warum du mit Elm programmieren solltest": https://medium.com/@markus_fit/how-i-fell-in-love-with-elm-6-reasons-you-should-too-6c6b4eadb42e