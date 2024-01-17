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

Bei der Programmierung kann es sehr nützlich sein, zu überprüfen, ob ein Verzeichnis existiert. Dies kann zum Beispiel verwendet werden, um sicherzustellen, dass bestimmte Dateien in einem bestimmten Verzeichnis vorhanden sind, bevor sie weiterverarbeitet werden. Es ist auch hilfreich, um sicherzustellen, dass das Programm auf die benötigten Ressourcen zugreifen kann.

## Wie geht's?

Fish Shell bietet eine einfache Möglichkeit, mit der "test" Befehl zu überprüfen, ob ein Verzeichnis existiert. Hier ist ein Beispiel:

```
test -d <Verzeichnisname>
```

Dieser Befehl gibt "wahr" zurück, wenn das Verzeichnis existiert und "falsch", wenn nicht. Man kann auch eine Bedingung hinzufügen, um zu überprüfen, ob das Verzeichnis tatsächlich lesbar ist:

```
test -d <Verzeichnisname> -a -r
```

Dieser Befehl gibt nur "wahr" zurück, wenn das Verzeichnis existiert und lesbar ist. Andernfalls wird "falsch" zurückgegeben.

## Tiefes Eintauchen

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Bestandteil der Dateiverwaltung und Sicherstellung der korrekten Programmabläufe. Es ist Teil der POSIX-Standards und wird von den meisten Shell-Kommandos unterstützt.

Eine alternative Möglichkeit, um zu überprüfen, ob ein Verzeichnis existiert, ist die Verwendung des "stats" Befehls. Dieser Befehl liefert detaillierte Informationen über eine Datei oder ein Verzeichnis, einschließlich der Existenz über einen bestimmten Dateipfad.

Die Implementierung des "test" Befehls in Fish Shell basiert auf der "test" Funktion des POSIX-Standards. Es gibt auch verschiedene Funktionen, die in Shell-Skripten verwendet werden können, um das Überprüfen, ob ein Verzeichnis existiert, zu erleichtern.

## Siehe auch

Weitere Informationen und Beispiele zur Verwendung des "test" Befehls in Fish Shell finden Sie in der offiziellen Dokumentation unter https://fishshell.com/docs/current/cmds/test.html. Für weitere Informationen zur Verwendung von "stats" können Sie die man-Page unter https://fishshell.com/docs/current/cmds/stat.html aufrufen.