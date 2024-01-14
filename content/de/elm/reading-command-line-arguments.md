---
title:                "Elm: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten kann für Elm-Entwickler*innen sehr hilfreich sein, um flexiblere und anpassungsfähigere Programme zu erstellen. Es ermöglicht die Verwendung von externen Eingaben zur Steuerung des Programms während der Laufzeit.

## Wie geht's

Um Befehlszeilenargumente in Elm zu lesen, können wir die `Platform.Cmd`-Bibliothek verwenden. Zuerst müssen wir sie importieren:

```Elm
import Platform.Cmd exposing (Cmd)
```

Dann können wir die Funktion `Platform.Cmd.args` verwenden, die eine `List String` zurückgibt. Diese Liste enthält alle Befehlszeilenargumente, die beim Ausführen des Programms angegeben wurden. Hier ist ein Beispielcode, der die Argumente ausgibt:

```Elm
main : program
main =
    Platform.worker
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> Sub.none
        }

init : () -> ( model, Cmd msg )
init _ =
    ( ()
    , Platform.Cmd.none
    )

update : msg -> model -> ( model, Cmd msg )
update _ model =
    ( model
    , Platform.Cmd.none
    )

view : model -> Html msg
view model =
    Html.text (List.toString (Platform.Cmd.args model))
```

Wenn wir unser Programm mit Befehlszeilenargumenten ausführen, zum Beispiel `elm make Main.elm -- --name John`, wird das Ergebnis `["--name", "John"]` sein.

## Tiefer Einblick

Es gibt auch die Möglichkeit, ein Flag für ein einzelnes Argument festzulegen, indem wir `Platform.Cmd.Arg String` verwenden. Hier ist ein Beispielcode, der das erste Befehlszeilenargument als Namen verwendet und es zusammen mit einer Standardnachricht in der HTML-Ausgabe anzeigt:

```Elm
view : model -> Html msg
view model =
    let
        name = case Platform.Cmd.arg 0 model of
            Platform.Cmd.Arg arg ->
                arg

            Platform.Cmd.NoArg ->
                "World"
    in
        Html.text ("Hello " ++ name)
```

Wenn wir das Programm ohne Argumente ausführen, wird die Ausgabe `Hello World` sein. Wenn wir jedoch ein Argument angeben, z.B. `elm make Main.elm --name John`, wird es als `Hello John` angezeigt.

## Siehe auch

- [Dokumentation zur Platform.Cmd-Bibliothek](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [GitHub-Repository von Elm](https://github.com/elm/compiler)