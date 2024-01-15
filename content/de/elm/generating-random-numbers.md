---
title:                "Zufallszahlen generieren"
html_title:           "Elm: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Situationen, in denen Programmierer zufällige Zahlen benötigen. Zum Beispiel beim Erstellen von Spielen oder bei der Durchführung von wissenschaftlichen Experimenten. In solchen Fällen können Sie nicht einfach vordefinierte Zahlen verwenden, sondern müssen eine Möglichkeit haben, zufällige Zahlen zu generieren. In diesem Artikel werden wir uns die Verwendung von Elm ansehen, um genau das zu tun - zufällige Zahlen generieren.

## Wie geht man vor?

Um zufällige Zahlen in Elm zu generieren, verwenden wir die Funktion `Random.generate`. Diese Funktion nimmt ein paar Argumente entgegen: einen Generator, der die Vorgehensweise zum Generieren der Zufallszahlen definiert, und einen Nachrichtentyp. Die Funktion gibt dann ein `Cmd msg` aus, das zur Verwendung in einem `update` in einer Elm Architecture verwendet werden kann. Schauen wir uns ein Beispiel an:

```Elm
import Random

-- Ein `Int`-Wert generieren
randomInt : Cmd msg
randomInt =
    Random.generate ReceivedRandomValue (Random.int 1 10)

-- Nachrichtentyp für den Rückgabewert
type Msg
    = ReceivedRandomValue Int

-- `update` Funktion mit Pattern Matching für die Nachricht
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceivedRandomValue value ->
            ( { model | randomValue = value}, Cmd.none )

-- `randomInt` in `init` verwenden
init : ( Model, Cmd Msg )
init = ( { randomValue = 0 }, randomInt )
```

In diesem Beispiel definieren wir die Funktion `randomInt`, die einen zufälligen `Int`-Wert zwischen 1 und 10 generiert und eine Nachricht vom Typ `ReceivedRandomValue` zurückgibt. In unserer `update` Funktion können wir dann über Pattern Matching auf diese Nachricht reagieren und den Wert in unserem Modell speichern. In `init` verwenden wir die Funktion `randomInt`, um den zufälligen Wert beim Starten unserer Anwendung zu generieren.

## Tiefergehende Informationen

Es gibt verschiedene Arten von Generatoren, die wir in der `Random`-Bibliothek von Elm verwenden können. Hier sind einige Beispiele:

- `Random.int` generiert einen zufälligen ganzzahligen Wert innerhalb eines angegebenen Intervalls.
- `Random.bool` generiert einen zufälligen Wahrheitswert (true oder false).
- `Random.float` generiert einen zufälligen Gleitkommawert zwischen 0 und 1.
- `Random.list` generiert eine Liste von zufälligen Werten basierend auf einem gegebenen Generator.
- `Random.generateList` ist ähnlich wie `Random.list`, ermöglicht jedoch das Angeben einer Größe für die Liste.

Es gibt auch Funktionen wie `Random.constant`, mit der wir einen bestimmten Wert als Ergebnis zurückgeben können, ohne einen Generator zu verwenden.

Weitere Informationen über die Verwendung von `Random` in Elm finden Sie in der offiziellen Dokumentation unter [https://package.elm-lang.org/packages/elm/random/latest/](https://package.elm-lang.org/packages/elm/random/latest/).

## Siehe auch

- [Offizielle Elm Dokumentation zur Verwendung von Random](https://package.elm-lang.org/packages/elm/random/latest/)
- [Beispielprojekt zum Generieren zufälliger Zahlen in Elm von Fun Fun Function](https://github.com/hochkind/elm-random-simple-example)