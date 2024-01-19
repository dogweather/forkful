---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Generierung von Zufallszahlen ist ein Prozess, bei dem ein Computerprogramm eine Zahl generiert, die den Anschein von Zufälligkeit hat. Dies ist wichtig, weil es in vielen Bereichen der Programmierung, wie beispielsweise bei der Erstellung zufälliger Passwörter oder bei Computerspielen, erforderlich ist.

## Wie machst du das:

Um in Elm Zufallszahlen zu generieren, verwenden wir die `Random` Bibliothek. Hier ist ein einfaches Beispiel:

```Elm
import Random
import Html exposing (Html, div, text)
import Task

main =
  Task.attempt (\_ -> Sub.none) (Task.succeed <| div [] [ text (toString <| Random.generate Random.constant 25) ])
```

Dieses Programm generiert beim Laden eine zufällige Zahl zwischen 0 und 25.

## Vertiefung

Die Generierung von Zufallszahlen hat eine lange Geschichte in der Informatik und ist ein Problem, das seit den Anfängen der Computerprogrammierung besteht. Es gibt verschiedene Ansätze und Algorithmen zur Generierung von Zufallszahlen, doch viele dieser Methoden sind nicht wirklich "zufällig". In Elm verwenden wir den Mersenne Twister Algorithmus, eine Methode, die für ihre hohe Qualität an Zufallszahlen bekannt ist. Als Alternative könnten wir auch eine externe API wie RANDOM.org benutzen, die echte Zufallszahlen aus atmosphärischem Rauschen erzeugt. Für die meisten Anwendungen ist dieser Grad an Zufälligkeit jedoch unnötig und der in Elm eingebettete Ansatz ausreichend.

## Siehe auch

- Random Bibliothek Dokumentation: https://package.elm-lang.org/packages/elm/random/latest/
- Mersenne Twister Algorithmus: https://de.wikipedia.org/wiki/Mersenne-Twister
- RANDOM.org API: https://www.random.org/clients/http/