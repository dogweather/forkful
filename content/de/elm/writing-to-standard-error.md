---
title:                "Schreiben auf den Standardfehler"
html_title:           "Elm: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man standard error (Standardfehler) in Elm schreiben würde. Der Hauptgrund ist, um Fehlermeldungen oder Fehlerinformationen anzuzeigen, wenn ein Programm ausgeführt wird. Dadurch können Entwickler*innen potenzielle Probleme schnell erkennen und beheben.

## Wie geht das?

Um standard error in Elm zu schreiben, gibt es zwei wichtige Schritte:

1. Importiere das `Debug` Modul von Elm in deiner Datei: `import Debug`.

2. Verwende die Funktion `crash` aus dem `Debug` Modul, um deine Fehlermeldung hinzuzufügen: `Debug.crash "Fehlermeldung hier"`.

Um die Fehlermeldung als standard error auszugeben, musst du deine Elm-Datei jetzt nur noch ausführen.

```Elm
import Debug

main =
    Debug.crash "Oops! Da ist etwas schief gelaufen."
```

Die obige Code-Snippet wird eine standard error Nachricht mit dem Inhalt "Oops! Da ist etwas schief gelaufen." ausgeben, wenn das Programm ausgeführt wird.

## Tiefes Eintauchen

Es gibt viele zusätzliche Optionen, die du beim Schreiben von standard error in Elm berücksichtigen kannst. Du kannst beispielsweise den `Debug` Kontext nutzen, um zusätzliche Informationen zu deinem Fehler hinzuzufügen. Außerdem kannst du auch ein benutzerdefiniertes `Results` Modul erstellen, um spezifischere Fehlermeldungen zu generieren.

## Siehe auch

- [Offizielle Elm Dokumentation](https://guide.elm-lang.org/error_handling/debugging.html)
- [Elm Forum Diskussion](https://discourse.elm-lang.org/t/error-handling-how-to-output-an-error-to-the-console/3131)
- [Github Issue Tracker](https://github.com/elm/compiler/issues/269)