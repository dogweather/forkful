---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:50.061545-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Generierung von Zufallszahlen bedeutet, im Programm nicht vorhersehbare Werte zu erzeugen, die für Spiele, Simulationen und Tests unentbehrlich sind. Programmierer nutzen sie, um ihren Code dynamisch und vielseitig zu machen.

## How to:
In Elm nutzt man das `Random` Modul, um Zufallszahlen zu erzeugen. Hier ein einfaches Beispiel:

```Elm
import Html exposing (Html, text)
import Random

main : Html msg
main =
    let
        seed = 42
        (randomNumber, newSeed) = Random.step (Random.int 1 100) (Random.initialSeed seed)
    in
    text (String.fromInt randomNumber)

-- Ausgabe: eine Zufallszahl zwischen 1 und 100
```
Das `Random.step` erzeugt sowohl einen Wert als auch einen neuen Seed für zukünftige Zufallszahlen.

## Tiefgang
Elm behandelt Zufallszahlen ganz anders als viele Sprachen. Da Elm eine reine funktionale Sprache ist, gibt es keine direkte Möglichkeit, bei jedem Durchlauf einen anderen Wert zu erhalten. Stattdessen nutzt man einen Seed, um Reproduzierbarkeit zu gewährleisten. Historisch gesehen ist dies wichtig für die Funktionsweise von "Zufälligkeit" in der funktionalen Programmierung.

Alternative Methoden zum Erzeugen von Zufallszahlen, wie die Nutzung eines externen Services oder die Implementierung eines benutzerdefinierten pseudozufälligen Zahlengenerators, sind möglich, aber in Elm nicht üblich.

In der Praxis verwendet man oft das `Random` Modul zusammen mit Subscriptions und Commands, um kontinuierlich Zufallszahlen innerhalb einer Anwendung zu generieren.

## Siehe Auch

- Elm Official Documentation on Random: [https://package.elm-lang.org/packages/elm/random/latest/](https://package.elm-lang.org/packages/elm/random/latest/)
- Elm Guide to Effects, covering Random: [https://guide.elm-lang.org/effects/random.html](https://guide.elm-lang.org/effects/random.html)