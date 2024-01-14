---
title:                "Elm: Verwendung regulärer Ausdrücke"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

Elm: Warum Sie sich mit regulären Ausdrücken beschäftigen sollten

Reguläre Ausdrücke sind ein mächtiges Werkzeug beim Entwickeln von Webanwendungen, insbesondere in der funktionalen Programmiersprache Elm. Sie können verwendet werden, um Muster in Texten zu erkennen und zu verarbeiten, was oft in der Datenvalidierung oder bei der Suche nach bestimmten Werten in einer Zeichenkette nützlich ist. In diesem Blog-Beitrag werden wir uns ansehen, warum Sie sich mit regulären Ausdrücken beschäftigen sollten und wie Sie sie in Elm verwenden können.

## Warum

Reguläre Ausdrücke bieten eine effiziente Möglichkeit, komplexe Aufgaben der Textverarbeitung zu automatisieren. Mit regulären Ausdrücken können Sie gezielt nach bestimmten Mustern in Zeichenketten suchen und diese entsprechend verarbeiten. Dies kann besonders nützlich sein, wenn Sie beispielsweise Formulareingaben validieren oder bestimmte Daten aus einer API-Antwort extrahieren möchten.

## How To

Um reguläre Ausdrücke in Elm zu verwenden, müssen Sie zuerst das Paket Regex installieren. Führen Sie dazu den folgenden Befehl in Ihrem Projektverzeichnis aus:

`elm install elm/regex`

Als nächstes importieren Sie das Modul `Regex` in Ihrer Elm-Datei:

```Elm
import Regex exposing (..)
```

Nun können Sie mit regulären Ausdrücken arbeiten. Ein Beispiel wäre die Validierung einer E-Mail-Adresse. Dazu könnten Sie folgenden Code verwenden:

```Elm
emailRegex : Regex.Pattern
emailRegex =
    Regex.fromRegex "[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}"

validateEmail : String -> Bool
validateEmail email =
    Regex.match emailRegex email
        |> Result.map (\_ -> True)
        |> Result.withDefault False

main =
    validateEmail "meine-email@beispiel.com" -- True
    validateEmail "ungültige-email" -- False
```

In diesem Beispiel verwenden wir das `fromRegex`-Funktion aus dem `Regex`-Modul, um ein reguläres Ausdrucksmuster zu erstellen, das einer gültigen E-Mail-Adresse entspricht. Dann verwenden wir die `match`-Funktion, um zu überprüfen, ob eine E-Mail-Adresse mit diesem Muster übereinstimmt. Das Ergebnis ist ein `Result Bool`, das angibt, ob die E-Mail-Adresse gültig ist oder nicht.

## Deep Dive

Es gibt viele verschiedene Operatoren und Funktionen, die in Elm reguläre Ausdrücke verwendet werden können. Einige davon sind `find`, `replace`, `replaceFirst`, `contains`, `split`, `matchAll` und viele mehr. Es ist ratsam, sich mit der offiziellen Dokumentation von `elm/regex` vertraut zu machen, um alle Möglichkeiten zu entdecken, die reguläre Ausdrücke bieten.

Zusätzlich zu den Standardoperatoren können Sie in Elm auch benutzerdefinierte Capture-Gruppen verwenden, um bestimmte Teile einer Übereinstimmung zu erfassen und in Verarbeitungsfunktionen zu verwenden. Dies kann besonders nützlich sein, wenn Sie Daten aus einer API-Antwort extrahieren oder formatieren möchten.

## Siehe auch

- Offizielle Dokumentation von `elm/regex`: https://package.elm-lang.org/packages/elm/regex/latest/
- Reguläre Ausdrücke in Elm: https://elm-lang.org/docs/strings#regular-expressions
- Einführung in reguläre Ausdrücke: https://regexone.com/