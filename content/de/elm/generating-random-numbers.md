---
title:                "Elm: Erzeugen von Zufallszahlen"
simple_title:         "Erzeugen von Zufallszahlen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man gelegentlich zufällige Zahlen in seinem Code benötigt. Zum Beispiel kann es sein, dass du ein Spiel entwickelst, bei dem du ein zufälliges Ereignis auslösen möchtest, oder du möchtest deinem Programm eine gewisse Dynamik verleihen, indem du zufällig generierte Werte verwendest. In jedem Fall ist es wichtig zu wissen, wie man in Elm Zufallszahlen generieren kann.

## Wie geht das?
In Elm gibt es das `Random`-Modul, das uns das Generieren von Zufallszahlen ermöglicht. Zunächst müssen wir dieses Modul importieren, indem wir folgende Zeile am Anfang unseres Codes einfügen:

```Elm
import Random
```

Dann können wir entweder eine zufällige ganze Zahl oder eine zufällige Gleitkommazahl generieren. Hier ist ein Beispiel für eine zufällige ganze Zahl zwischen 1 und 10:

```Elm
Random.generate Random.int 1 10
--> 7
```

Und hier ist ein Beispiel für eine zufällige Gleitkommazahl zwischen 0 und 1:

```Elm
Random.generate Random.float 0 1
--> 0.7438
```

Du kannst auch ein benutzerdefiniertes Zufallszahlengenerator-Funktion erstellen, indem du die `batch`-Funktion verwendest. Hier ist ein Beispiel, in dem wir eine zufällige Zahlengruppe bestimmen, die eine Farbe repräsentiert:

```Elm
type alias Color =
    { red : Int
    , green : Int
    , blue : Int
    }

randomColor : Random.Generator Color
randomColor =
    Random.batch
        [ Random.int 0 255
        , Random.int 0 255
        , Random.int 0 255
        ]
        |> Random.map3 Color

Random.generate randomColor
--> { red = 152, green = 218, blue = 99 }
```

## Tief eintauchen
Die `Random`-Modul bietet auch Methoden, mit denen du Zufallszahlen in deinen Programmen in verschiedenen Situationen verwenden können. Zum Beispiel gibt es die `andThen`-Funktion, die es dir ermöglicht, eine zufällige Zahl mit einem bestimmten Wert zu kombinieren und die `generateSeed`-Funktion, mit der du einen benutzerdefinierten Zufallszahlengenerator erstellen kannst.

Wenn du mehr über die verschiedenen Funktionen des `Random`-Moduls erfahren möchtest, empfehle ich dir, die offizielle Dokumentation zu lesen.

## Siehe auch
- [Offizielle Elm Dokumentation zum Random-Modul](https://package.elm-lang.org/packages/elm/random/latest/)
- [Blogpost über Zufallszahlen in Elm von Breaking the Jarg](https://blog.breaking-the-jarg.heroku.com/random-numbers-in-elm/)