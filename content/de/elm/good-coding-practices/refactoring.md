---
title:                "Refactoring"
date:                  2024-01-26T01:17:58.158354-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist im Grunde genommen wie ein Frühjahrsputz für Ihre Codebasis – es geht darum, bestehenden Code umzustrukturieren, ohne sein externes Verhalten zu ändern. Programmierer tun dies, um den Code lesbarer zu machen, Komplexität zu reduzieren, die Wartbarkeit zu verbessern und ihn einfacher erweiterbar zu machen.

## Wie geht das:
Angenommen, Sie haben eine Elm-Funktion, die zu viel macht, wie zum Beispiel UI-Logik mit Statusaktualisierungen zu vermischen. Sie ist ein perfekter Kandidat für das Refactoring. Ursprünglich:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

Nach dem Refactoring trennen wir die Anliegen, indem wir die Logik in verschiedene Funktionen auslagern:

```Elm
-- Die Aktualisierungslogik ist separat
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- Die Formatierungs- (Ansichts-) Logik ist auch separat
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Eingabe löschen, wenn sie zu kurz ist, als ein Beispielregel.

-- Die Aktualisierungsfunktion verwendet jetzt Hilfsfunktionen
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Mit diesen Änderungen haben Sie eine klare Trennung, und jede Funktion ist einfacher zu verstehen und zu testen.

## Tiefere Einblicke
Refactoring als formale Praxis kann bis in die Anfangstage der Programmierung zurückverfolgt werden, als die Kosten für die Änderung von Code bereits als ein kritischer Aspekt des Entwicklungsprozesses erkannt wurden. Insbesondere hat das Buch von Martin Fowler "Refactoring: Verbesserung des Designs bestehender Code", das Ende der 1990er Jahre veröffentlicht wurde, den Weg für das Refactoring mit einem strukturierten Ansatz und einem Katalog von "Code-Gerüchen" zur Identifizierung von Refactoring-Möglichkeiten wirklich geebnet.

Im Kontext von Elm nutzt das Refactoring die Stärken der Sprache, wie ihr starkes Typsystem, das während des Prozesses Vertrauen fördert. Alternativen zum manuellen Refactoring können automatisierte Code-Transformationstools umfassen, aber die Werkzeuge von Elm in diesem Bereich sind im Vergleich zu einigen älteren Sprachen noch in der Reifephase. Implementierungsdetails drehen sich oft um gängige Refactorings wie die Extraktion von Funktionen, Umbenennung und Vereinfachung von Bedingungen. Der Elm-Compiler ist ein wichtiger Verbündeter beim Refactoring, da er nicht viel durchgehen lässt – er schreit, wann immer etwas fehlt und stellt sicher, dass Ihr refaktorisierter Code noch funktioniert.

## Siehe auch
- ["Refactoring: Verbesserung des Designs bestehender Code" von Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Elm Diskurs – Themen zum Refactoring](https://discourse.elm-lang.org/search?q=refactoring)
