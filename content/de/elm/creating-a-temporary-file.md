---
title:                "Erstellen einer temporären Datei"
html_title:           "Elm: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Erstellung einer temporären Datei beschäftigen? Nun, temporäre Dateien sind nützlich, wenn man Daten speichern oder verarbeiten möchte, die nur vorübergehend gebraucht werden. Sie sind besonders hilfreich bei der Verarbeitung von großen Mengen an Daten oder wenn man bestimmte Prozesse automatisieren möchte.

## So geht's

Um eine temporäre Datei in Elm zu erstellen, kann man die `Temporary`-Bibliothek verwenden. Zunächst müssen wir diese Bibliothek zu unserem Projekt hinzufügen, indem wir folgenden Befehl in der Elm REPL eingeben:

```Elm
elm install elm-lang/temporary
```

Sobald die Bibliothek installiert ist, können wir sie in unserem Code importieren:

```Elm
import Temporary
```

Als nächstes müssen wir die Funktion `Temporary.file` aufrufen, um eine temporäre Datei zu erstellen. Diese Funktion erwartet zwei Argumente: den Dateinamen und den Inhalt der Datei.

```Elm
Temporary.file "meine_datei.txt" "Dies ist der Inhalt meiner temporären Datei."
```

Dieser Aufruf erstellt eine temporäre Datei mit dem Namen "meine_datei.txt" und dem angegebenen Inhalt. Die Datei wird im Standard-Temporärordner des Betriebssystems erstellt.

Um den Pfad zur erstellten Datei zu erhalten, können wir die Funktion `Temporary.path` verwenden:

```Elm
let
    datei = Temporary.file "meine_datei.txt" "Dies ist der Inhalt meiner temporären Datei."
in
    Temporary.path datei
```

## Tiefere Einblicke

Die `Temporary`-Bibliothek verwendet interne Plattform-Tools, um temporäre Dateien zu erstellen. Sie bietet auch einige erweiterte Funktionen, wie die Möglichkeit, einen bestimmten Ordner für die Erstellung der temporären Datei anzugeben.

Es ist wichtig zu beachten, dass temporäre Dateien nicht für die langfristige Speicherung von Daten gedacht sind. Sie werden in der Regel automatisch gelöscht, wenn das Programm beendet wird. Wenn man also Daten langfristig speichern möchte, sollte man besser andere Techniken verwenden, wie zum Beispiel das Speichern von Daten in einer Datenbank.

## Siehe auch

- [Elm Dokumentation über die Temporary-Bibliothek](https://package.elm-lang.org/packages/elm-lang/temporary/latest/)
- [Ein Artikel über Dateiverwaltung in Elm](https://www.elm-tutorial.org/de/05-deine-erste-elm-app/03-datei-verarbeiten.html)
- [Weitere Informationen über das Speichern von Daten in Elm](https://guide.elm-lang.org/effects/file_system.html)