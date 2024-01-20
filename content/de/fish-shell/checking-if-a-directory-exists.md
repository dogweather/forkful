---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist im wesentlichen eine Methode, mit der ein Programm bestimmt, ob ein bestimmter Pfad in einem Dateisystem zu einem Verzeichnis führt oder nicht. Programmierer verwenden diese Methode, um sicherzustellen, dass sie nicht versuchen, Dateioperationen in einem nicht vorhandenen Verzeichnis durchzuführen, was zu Fehlern führen würde.

## So geht's

In der Fish Shell machen Sie das folgendermaßen:

```Fish Shell
if test -d /dein/pfad
    echo 'Das Verzeichnis existiert!'
else
    echo 'Das Verzeichnis existiert nicht!'
end
```

Ein Beispiel für die Ausgabe könnte sein:

```Fish Shell
Das Verzeichnis existiert!
```

Prüfen Sie es selbst. Ersetzen Sie "/dein/pfad" durch den Pfad, den Sie überprüfen möchten.

## Hintergrund

Historisch betrachtet stammt die Methode zur Überprüfung der Existenz eines Verzeichnisses von Unix- und Linux-Systemen. "test" ist ein Builtin-Befehl in unixoiden Betriebssystemen wie Linux, macOS und BSD.

Als Alternative könnten Sie auch den Builtin-Befehl `stat` verwenden. Allerdings ist der "test" -Befehl leichter zu nutzen und der Standard in den meisten Skripten.

Die Implementierung dieser Methode in der Fish Shell überprüft einfach, ob das Betriebssystem den Pfad als ein Verzeichnis anerkennt. 

## Siehe Auch

Um mehr über die Fish Shell und ihre verschiedenen Funktionen zu erfahren:

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Unix test Befehl](https://man7.org/linux/man-pages/man1/test.1.html)
- [Fish Shell GitHub](https://github.com/fish-shell/fish-shell)