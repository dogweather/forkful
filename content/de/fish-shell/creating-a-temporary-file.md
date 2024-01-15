---
title:                "Erstellen einer temporären Datei"
html_title:           "Fish Shell: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man überhaupt einen temporären Datei erstellen? Nun, es gibt verschiedene Situationen, in denen dies notwendig sein könnte. Zum Beispiel kann es erforderlich sein, temporäre Dateien zu verwenden, um Daten zwischen verschiedenen Programmen zu übertragen oder um vorübergehende Speicherung für bestimmte Daten zu schaffen.

## Wie geht's?

Das Erstellen einer temporären Datei mit Fish Shell ist ziemlich einfach. Dazu können wir das Befehl "mktemp" verwenden, der speziell für diesen Zweck entwickelt wurde.

```Fish Shell

set temp_file (mktemp) 

```

Das obige Beispiel erstellt eine temporäre Datei mit einem zufälligen Namen und speichert den Pfad zur Datei in einer Variablen namens "temp_file". Um jetzt Daten in diese temporäre Datei zu schreiben, können wir den Befehl "echo" verwenden.

```Fish Shell

echo Hallo Welt > $temp_file

```

Dieser Befehl schreibt den Text "Hallo Welt" in die temporäre Datei. Um den Inhalt der Datei anzuzeigen, können wir einfach den Inhalt der Variablen "temp_file" ausgeben.

```Fish Shell

echo $temp_file

```

Dies sollte den Pfad zur temporären Datei ausgeben, zusammen mit dem Text "Hallo Welt", den wir zuvor hinzugefügt haben.

## Tiefer gehende Informationen

Beim Erstellen einer temporären Datei mit Fish Shell gibt es ein paar Dinge zu beachten. "mktemp" erstellt automatisch eine temporäre Datei in einem temporären Ordner, und löscht die Datei automatisch, wenn das Skript beendet wird. Wenn Sie jedoch möchten, dass die temporäre Datei an einem bestimmten Speicherort erstellt wird, können Sie dies mit dem Befehl "mktemp -d" angeben und einen spezifischen Pfad angeben.

Eine weitere Sache zu beachten ist, dass "mktemp" eine zufällige Datei mit einem zufälligen Namen erstellt, was bedeutet, dass jedes Mal, wenn Sie den Befehl ausführen, eine neue Datei erstellt wird. Dies kann nützlich sein, wenn Sie mehrere temporäre Dateien benötigen, aber wenn Sie eine bestimmte Datei immer wieder verwenden möchten, sollten Sie sie mit einem spezifischen Namen erstellen und darauf achten, dass sie nicht überschrieben wird.

## Siehe auch

- Offizielle Fish Shell Dokumentation: https://fishshell.com/docs/current/
- Kurze Einführung in Fish Shell: https://dev.to/bowmanjd/a-quick-introduction-to-fish-shell-4ge5
- Weitere Beispiele und Tipps zur Verwendung von Fish Shell: https://medium.com/@rommelvs007/fish-shell-59bd8b9af1f6