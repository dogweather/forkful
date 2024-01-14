---
title:                "Elm: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist ein wesentlicher Bestandteil des Entwicklungsprozesses in der Welt des Programmierens. Wenn Sie jedoch in Elm programmieren, kann das Hinzufügen von zusätzlichen Debug-Ausgaben die Fehlersuche erleichtern und Ihnen helfen, ein besseres Verständnis für Ihre Anwendung zu entwickeln. In diesem Blog-Beitrag werden wir uns ansehen, warum es wichtig ist, Debug-Ausgaben in Elm zu verwenden.

## Wie

Um Debug-Ausgaben in Elm zu implementieren, müssen wir die Funktion `Debug.log` verwenden. Lassen Sie uns ein einfaches Beispiel betrachten:

````Elm
import Debug

name = "Max"
Debug.log "Mein Name ist" name
````

Der obige Code erstellt zunächst eine Variable `name` mit dem Wert "Max". Anschließend verwenden wir die `Debug.log` Funktion, um unseren Namen als String in die Konsole auszugeben. Wenn wir die Anwendung ausführen, sehen wir die folgende Ausgabe in unserer Konsole:

Mein Name ist Max

Durch die Verwendung von `Debug.log` können wir also die Werte von Variablen in unserer Anwendung ausgeben und überprüfen, ob sie den erwarteten Wert haben. Dies kann uns helfen, mögliche Fehlerquellen zu identifizieren und unseren Code zu verbessern.

## Deep Dive

Neben der `Debug.log` Funktion gibt es noch weitere Möglichkeiten, Debug-Ausgaben in Elm zu nutzen. Eine davon ist die Verwendung von `Debug.watch`, um Änderungen an einer bestimmten Variable zu überwachen. Hier ein Beispiel:

````Elm
import Debug

sum = 10 + 20
Debug.watch "Die Summe ist" sum
````

In diesem Fall wird die Ausgabe "Die Summe ist 30" angezeigt. Wenn wir nun den Wert von `sum` ändern, z.B. auf 50, wird die Ausgabe in unserer Konsole entsprechend aktualisiert. Dies ist besonders nützlich, wenn wir bestimmte Werte oder Berechnungen in unserer Anwendung genau beobachten möchten.

Eine weitere Methode ist die Verwendung von `Debug.crash`, um eine Fehlermeldung mit benutzerdefiniertem Text auszugeben. Dies kann vor allem bei der Fehlersuche in komplexen Anwendungen hilfreich sein.

## Siehe auch

- Offizielle Elm Dokumentation zu Debugging: https://guide.elm-lang.org/debugging/
- Interaktiver Online-Debugger für Elm: https://elm-lang.org/try