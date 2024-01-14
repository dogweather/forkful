---
title:    "Elm: Ein neues Projekt beginnen."
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Wenn du daran interessiert bist, eine neue Programmiersprache zu lernen, die einfach und funktional ist, dann ist Elm vielleicht genau das Richtige für dich. Elm ermöglicht es dir, schnell und effizient Webanwendungen zu erstellen, die stabil, sicher und leicht zu warten sind. In diesem Artikel erfährst du, wie du ein neues Projekt in Elm starten kannst.

## Wie geht's

Um ein neues Projekt in Elm zu starten, musst du zunächst die Sprache und ihre Syntax verstehen. Hier ist ein einfaches Beispiel, das eine Liste mit Namen von Personen erstellt und sie auf der Konsole ausgibt:

```Elm
-- Definition einer Liste von Namen
names = ["Lisa", "Max", "Anna"]

-- Funktion, die die Liste auf der Konsole ausgibt
printNames list = 
    List.map (\name ->
        "Hallo " ++ name
    ) list
        |> Debug.log "Namen:"
```

Dieses Beispiel zeigt dir den grundlegenden Aufbau einer Elm-Anwendung: Definition von Variablen, Funktionen und die Verwendung von Pipelines.

## Tiefentauchen

Wenn du ein neues Projekt in Elm startest, gibt es ein paar Dinge zu beachten. Zunächst solltest du dir über die Struktur deiner Anwendung Gedanken machen. In Elm besteht jede Anwendung aus einer "Main"-Funktion, die das gesamte Programm verwaltet. Diese Funktion sollte wiederum aus Unterfunktionen bestehen, die jeweils für eine bestimmte Aufgabe zuständig sind.

Es ist auch wichtig, die Module in deinem Projekt richtig zu organisieren. In Elm gibt es keine Klassen, stattdessen werden Funktionen und Typen in Modulen gruppiert. Dadurch wird dein Code übersichtlicher und leichter zu warten.

Schließlich solltest du beim Schreiben von Elm-Code immer darauf achten, dass du funktionale Konzepte wie Immutabilität und unveränderliche Datenstrukturen einhältst. Dadurch wird der Code stabiler, sicherer und leichter zu debuggen.

## Siehe auch

- Offizielle Elm Dokumentation: https://guide.elm-lang.org/
- Elm Packages: https://package.elm-lang.org/
- Elm Forum: https://discourse.elm-lang.org/