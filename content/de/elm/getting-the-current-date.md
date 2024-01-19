---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Als Programmierer holen wir oft das aktuelle Datum und die Uhrzeit ab. Dies hilft uns, wichtige Zeitschritte zu markieren, Ereignisse zu protokollieren, oder auch um abhängige Funktionen wie Countdowns zu erstellen.

## Wie zu:

Um das aktuelle Datum in Elm zu bekommen, verwenden wir die `Time.now` Funktion innerhalb einer `Task`, gefolgt von `Task.perform` um die Task auszuführen:

```Elm 
import Time exposing (Posix, second, toTime)

type alias Model =
    { time : Maybe Posix
    }

init : ( Model, Cmd Msg )
init =
    ( { time = Nothing }
    , Task.perform TimeUpdate Time.now
    )

type Msg
    = TimeUpdate Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate newTime ->
            ( { model | time = Just newTime }
            , Cmd.none
            )
```

Wenn das Programm läuft, wird `Time.now` aufgerufen und speichert das aktuelle Datum in Millisekunden seit der Unix-Ära (1. Januar 1970). 

## Tiefere Erklärung:

Das Konzept der Zeit in Computerprogrammen ist seit den Anfängen der Computersoftware weit verbreitet. Doof nur, dass Elm rein funktionell ist und sich damit bedeckt hält, Zustände und Seiteneffekte zu verwalten, wie die aktuelle Zeit und Datum. Dafür verwendet Elm die `Task` um asynchrone arbeiten, wie die Anforderung der aktuellen Zeit, zu managen.

Es gibt Alternativen zur `Time.now` Funktion, z.B. `Time.utc`, welche die Zeit in UTC zurückgibt, oder `Time.posixToMillis`, welcher das Datum vom Typ `Posix` zu Millisekunden konvertiert.

## Weiterführende Links:

1. Elm Time Dokumentation: http://package.elm-lang.org/packages/elm/time/latest/
2. Elm Task Beispiele: https://elmprogramming.com/tasks.html
3. Funktionsweise des Unix Zeitstempels: https://de.wikipedia.org/wiki/Unixzeit