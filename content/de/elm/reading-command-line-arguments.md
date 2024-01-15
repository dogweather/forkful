---
title:                "Das Lesen von Befehlszeilenargumenten"
html_title:           "Elm: Das Lesen von Befehlszeilenargumenten"
simple_title:         "Das Lesen von Befehlszeilenargumenten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Lesen von Befehlszeilenargumenten beschäftigen? Nun, wenn du schon immer die volle Kontrolle über deine Elm Programme haben wolltest, bist du hier genau richtig. Durch das Lesen von Befehlszeilenargumenten kannst du die Funktionalität deiner Programme erweitern und an die Bedürfnisse deiner Benutzer anpassen.

## Wie geht das?

Um Befehlszeilenargumente in Elm zu lesen, müssen wir die eingebaute Funktion ```platform.programWithFlags``` verwenden. Diese Funktion ermöglicht es uns, Daten von der Befehlszeile zu empfangen und sie in unserer Elm Anwendung zu verwenden.

Hier ist ein Beispiel, wie du ```platform.programWithFlags``` verwenden kannst, um die Anzahl der übergebenen Argumente zu zählen und dann die Argumente auszugeben:

```
Elm.Application.platform = platform
myProgram : Program Never Model
myProgram =
    platform.programWithFlags
        model
        (Elm.Platform.programWithFlagsParser argParser)

type alias Model =
    { args : List String
    , numArgs : Int
    }

argParser : Parser ( List String )
argParser =
    Parser.sequence
        (List.repeat numArgs Parser.string)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Init flags ->
            ( { model | args = flags
              , numArgs = List.length flags
              }
            , Cmd.none
            )

        ...

view : Model -> Html Msg
view model =
    div []
        [ text ("Anzahl der Argumente: " ++ String.fromInt model.numArgs)
        , ul []
            (List.map (\arg -> li [] [text arg]) model.args)
        ]
```

Die Ausgabe dieses Programms würde folgendermaßen aussehen:

```
> elm-reactor -p 8000
Anzahl der Argumente: 2
-p
8000
```

## Tiefgang

Jetzt wo du gesehen hast, wie einfach es ist, Befehlszeilenargumente in Elm zu lesen, gibt es noch ein paar Dinge, die du beachten solltest.

Erstens: Es ist wichtig zu wissen, dass die Anzahl der Argumente möglicherweise nicht immer mit der Anzahl der übergebenen Argumente übereinstimmt, da einige Argumente von der Elm Plattform selbst bereitgestellt werden (z.B. ```--port```).

Zweitens: Du kannst deine Argumente auch in benutzerdefinierte Typen parsen, anstatt sie als ```List String``` zu behandeln. Dies ermöglicht es dir, spezifischere Daten aus den Argumenten zu extrahieren.

Und schließlich: Es gibt auch die Möglichkeit, benutzerdefinierte Flags in deiner Elm Anwendung zu verwenden, anstatt sie nur aus der Befehlszeile zu lesen. Diese Flags können sogar von anderen Modulen innerhalb deiner Anwendung verwendet werden.

## Siehe auch

- Die offizielle Elm Dokumentation zu Befehlszeilenargumenten: https://elm-lang.org/blog/farewell-to-flags
- Ein Beispielprojekt zum Lesen von Befehlszeilenargumenten in Elm: https://github.com/ryan-senn/elm-cli-arguments/tree/master/examples/short-options