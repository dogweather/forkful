---
title:                "Schreiben auf Standardfehler"
html_title:           "Elm: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf den Standardfehler ist ein Weg für Programmierer, um Informationen oder Fehlermeldungen auf der Konsole auszugeben, anstatt auf die Standardausgabe. Dies ist nützlich, um Fehler und Probleme zu identifizieren, insbesondere während der Entwicklung oder beim Debuggen von Code.

## Wie geht's?
Um auf den Standardfehler zu schreiben, können wir die `Debug.log` Funktion verwenden. Diese Funktion akzeptiert zwei Argumente: einen String mit der gewünschten Meldung und einen Wert zum Ausgeben. Hier ist ein Beispiel, wie man sie benutzt:

```Elm
Debug.log "Benutzername:" "John Smith"
```
Das würde `Benutzername: John Smith` auf der Konsole ausgeben.

## Tief tauchen
Das Schreiben auf den Standardfehler ist nicht neu und wird schon seit langer Zeit von Programmiersprachen wie C und Java verwendet. Es ist eine schnelle und einfache Methode, um Informationen auszugeben und Fehler zu beheben. Eine Alternative zur Verwendung von `Debug.log` wäre das Verwenden von `Console.error`, das von der Elm-Bibliothek bereitgestellt wird. Dieses Modul bietet mehrere Funktionen, um auf die Konsole zu schreiben, einschließlich des Schreibens auf den Standardfehler.

## Siehe auch
- [Die Elm-Website](https://elm-lang.org/) für mehr Informationen zur Sprache und ihrer Verwendung.
- [Die offizielle Elm-Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/) für weitere Details zur `Debug`- und `Console`-Module.
- [Ein Tutorial zur Elm-Programmierung](https://guide.elm-lang.org/) für Anfänger.