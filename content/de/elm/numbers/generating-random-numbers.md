---
title:                "Generierung von Zufallszahlen"
aliases: - /de/elm/generating-random-numbers.md
date:                  2024-01-27T20:33:44.745303-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Generieren von Zufallszahlen in Elm beinhaltet die Erstellung unvorhersehbarer numerischer Werte, die für Anwendungen wie Spiele, Simulationen und Sicherheitsalgorithmen unerlässlich sind. Programmierer verwenden Zufälligkeit, um reale Variabilität zu simulieren, die Benutzererfahrung zu verbessern oder Daten mit Verschlüsselungstechniken zu sichern.

## Wie:
Elm geht anders als viele Programmiersprachen mit Zufälligkeit um und nutzt ein System, das Funktionen rein hält. Um Zufallszahlen zu generieren, müssen Sie mit dem `Random`-Modul von Elm arbeiten. Hier ist ein einfaches Beispiel, wie eine Zufallszahl zwischen 1 und 100 generiert wird:

```Elm
import Html exposing (Html, text)
import Random

main : Html msg
main =
    Random.generate NewRandomNumber (Random.int 1 100)
    |> Html.map (text << toString)

type Msg = NewRandomNumber Int
```

Dieser Schnipsel verwendet `Random.generate`, um einen Befehl zu erstellen, der bei der Ausführung eine Zufallszahl im angegebenen Bereich produziert. Die `type Msg`-Deklaration wird verwendet, um die generierte Zahl in der Update-Funktion Ihrer Elm-Anwendung zu behandeln.

Für ein interaktiveres Beispiel sehen wir uns ein Szenario an, in dem Benutzer durch einen Klick die Generierung von Zufallszahlen auslösen:

```Elm
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random

type alias Model = Int

type Msg = Generate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            (model, Random.generate NewRandomNumber (Random.int 1 100))

view : Model -> Html Msg
view model =
    div []
        [ text ("Generierte Zahl: " ++ String.fromInt model)
        , button [ onClick Generate ] [ text "Neue Zahl generieren" ]
        ]

type Msg = NewRandomNumber Int
```

Diese Elm-Anwendung führt Interaktivität ein, indem das Display mit einer neuen Zufallszahl aktualisiert wird, jedes Mal wenn der Benutzer den Knopf klickt.

## Tiefergehender Einblick
Das Design von Elms System zur Generierung von Zufallszahlen resultiert aus dem Engagement der Sprache für Reinheit und Vorhersagbarkeit. Anstatt direkte, unreine Funktionen, die bei jedem Aufruf unterschiedliche Werte zurückgeben, kapselt Elm Zufälligkeit in einer `Cmd`-Struktur ein, im Einklang mit seiner Architektur, die Nebenwirkungen von reinen Funktionen trennt.

Obwohl dieser Ansatz Konsistenz im Verhalten der Anwendung garantiert und das Debugging erleichtert, führt er eine Lernkurve für diejenigen ein, die an die imperative Generierung von Zufallszahlen gewöhnt sind. Jedoch überwiegen die Vorteile der Aufrechterhaltung der Reinheit der Anwendung und die Leichtigkeit des Testens oft die anfängliche Komplexität.

Elms Methode steht im Gegensatz zu Sprachen, die globale Generatoren für Zufallszahlen bieten, was zu subtilen Fehlern aufgrund von gemeinsam genutzten Zuständen führen kann. Indem Elm eine explizite Handhabung der Generierung von Zufallszahlen und ihrer Effekte erfordert, ermutigt es Entwickler, kritischer darüber nachzudenken, wo und wie Zufälligkeit ihre Anwendungen beeinflusst, was zu robusterem und vorhersagbarerem Code führt.

Als Alternativen bieten andere funktionale Sprachen ähnliche Funktionalitäten, können diese jedoch unterschiedlich implementieren. Haskell hält beispielsweise auch die Reinheit bei der Generierung von Zufallszahlen aufrecht, aber durch die Verwendung von Monaden, ein Konzept, das Elm absichtlich vermeidet, um sein Modell zu vereinfachen. Im Vergleich ist Elms Ansatz für Neulinge zugänglicher und betont eine unkomplizierte Anwendungsarchitektur, ohne die Kraft der Prinzipien der funktionalen Programmierung zu opfern.
