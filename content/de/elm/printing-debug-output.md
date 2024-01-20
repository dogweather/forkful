---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Debug-Ausgabe drucken ist die Methode zur Kontrolle und Überwachung des Programmablaufs zur Fehlersuche. Programmierer tun dies, um verborgene Fehler oder unerwartete Verhaltensweisen in ihrer Software zu entdecken und zu beheben.

## So geht's:

In Elm benutzen wir das `Debug.log` Werkzeug, um Debug-Ausgaben zu drucken. Hier ist ein einfacher Beispielcode:

```Elm
import Html exposing (Html, text)
import Debug

main =
    Debug.log "ich bin ein Debug-Statement" (text "Hello Elm World!")
```

Laufen Sie dieses Programm und Sie werden sehen, im Konsolenfenster wird "ich bin ein Debug-Statement" und "Hello Elm World!" ausgegeben.

## Tiefer Eintauchen:

Das `Debug.log` Werkzeug hat eine lange Geschichte in der Programmiersprache Elm und ist ein unverzichtbares Werkzeug für Entwickler. Es bietet eine einfache Methode, um Informationen über den Zustand Ihres Programms während der Ausführung auszugeben.

Es gibt Alternativen wie die Nutzung von `Debug.todo`, aber diese hat einen anderen Zweck. `Debug.todo` zwingt Sie zu einer späteren Umsetzung, weil das Programm sonst nicht kompiliert.

`Debug.log` implementiert wird, indem eine Funktion erstellt wird, die eine Zeichenkette und einen Wert erhält. Die Funktion gibt dann den Wert unverändert zurück, aber sie druckt auch die Zeichenkette und eine Darstellung des Werts auf die Konsole.

## Siehe Auch:

Für mehr Informationen über `Debug.log` und andere ähnliche Funktionen in Elm, checken Sie die offiziellen Elm Dokumentation: https://package.elm-lang.org/packages/elm/core/latest/Debug

Für eine detailliertere Diskussion darüber, wie man Debugging in Elm effektiv nutzt, schauen Sie sich diese großartige Ressource an: https://elmprogramming.com/debugging.html