---
title:                "Elm: Ausgabe von Debug-Informationen drucken"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Ausgeben von Debug-Ausgaben kann ein nützliches Werkzeug sein, um Fehler in unserer Elm-Programmierung zu finden. Oftmals ist es schwierig, den genauen Grund für einen Fehler zu erkennen, aber durch das Ausgeben von Informationen können wir auf unsere Funktionen und Variablen zugreifen und sehen, was während der Ausführung tatsächlich passiert: Schritt für Schritt.

## Wie man Debug-Ausgaben erstellt

Um Debug-Ausgaben zu erstellen, können wir die "Debug.log" Funktion von Elm verwenden. Diese Funktion akzeptiert eine Beschreibung in Form eines Strings und eine beliebige Wert als Argument, den wir ausgeben möchten.

````Elm
import Debug

main =
  model
    |> Debug.log "Model vor dem Update" 
    |> update msg 
    |> Debug.log "Model nach dem Update" 
````

Die obige Beispielcode zeigt, wie wir die "Debug.log" Funktion verwenden, um Informationen über den Zustand unserer Modell vor und nach einem Update auszugeben.

Die Ausgabe wird im Browser-Entwicklertools-Konsolenfenster angezeigt und sieht folgendermaßen aus:

````Elm
Model vor dem Update: { … }
Model nach dem Update: { … }
````

## Tiefer Einblick

Neben der einfachen Verwendung von "Debug.log" können wir auch komplexe Debug-Ausgaben erstellen, indem wir uns mit der "Debug.todo" Funktion beschäftigen. Diese Funktion ist in Elm integriert, um uns daran zu erinnern, dass bestimmte Teile unseres Codes noch nicht implementiert oder von uns noch nicht ausgefüllt sind.

````Elm
import Debug

-- Alle Namen in diesem String durch einen Namen einer unserer Funktionen ersetzen
main = Debug.todo "Function Name Here"
````

Wir können auch bestimmte Variablen oder Funktionen in unserer Debug-Meldung angeben, um ihnen einen genaueren Einblick zu geben. 

````Elm
import Debug

-- Eine Debug-Ausgabe mit konkreter Information darüber, 
-- was wir ausgeben möchten.
main =
  vonizioLog
    |> Debug.log "Was die 'vonizioLog' hält: <<Hier Daten hier>>"
````

## Siehe auch
- [Elm Debugging Guide: So finden wir den Debug Inhalt - ElmCast (EN)](https://elmcast.io # debugging # elm -how -to # per-der -debug -output) 
- [Debug-Info für Ansichten, listen und Taste verfügbar (EN)](https://kitgui.com / blog/2015/09/17/debug -info -for -Views -Lists -and -Effects -Available /) 
- [Debug Ausgabe machinem Beweis (EN)](https://www.youtube.com/watch?v=kP4zaOhSM4o )