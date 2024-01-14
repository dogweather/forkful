---
title:                "Fish Shell: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Oftmals müssen wir in unserer Programmierarbeit überprüfen, ob ein bestimmtes Verzeichnis existiert oder nicht. Dies kann hilfreich sein, um sicherzustellen, dass unsere Anwendung ordnungsgemäß funktioniert oder um besondere Aktionen auszuführen, abhängig davon, ob das Verzeichnis vorhanden ist oder nicht.

# Wie geht das

Um zu überprüfen, ob ein bestimmtes Verzeichnis existiert, können wir in Fish Shell den Befehl `test -d` verwenden. Dieser Befehl prüft, ob das angegebene Verzeichnis existiert oder nicht und gibt entsprechend `true` oder `false` zurück.

Hier ist ein Beispiel, in dem wir überprüfen, ob das Verzeichnis `~/Dokumente` existiert:

```Fish Shell
test -d ~/Dokumente
echo $status # Output: 0 (true)
```

Wir können auch eine if-else-Bedingung verwenden, um entsprechend zu handeln:

```Fish Shell
if test -d ~/Dokumente
    echo "Das Verzeichnis existiert."
else
    echo "Das Verzeichnis existiert nicht."
end
```

# Tiefgehende Analyse

Um tiefer in die Funktionsweise des Befehls `test -d` einzutauchen, müssen wir uns mit dem Unix-Dateisystem befassen. In Unix-Systemen gibt es eine Verzeichnisstruktur, die als Baumstruktur organisiert ist. Jeder Knoten in diesem Baum ist ein Verzeichnis. Durch die Verwendung von relativen oder absoluten Pfaden können wir auf bestimmte Verzeichnisse zugreifen.

Der Befehl `test -d` überprüft nun, ob der angegebene Pfad tatsächlich ein Verzeichnis ist oder nicht. Wenn der Pfad zu einem Verzeichnis führt, gibt der Befehl `true` zurück, ansonsten `false`.

# Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Unix Dateisystem](https://www.lifewire.com/unix-file-system-2788359)
- [Bash Befehl zum Überprüfen, ob ein Verzeichnis existiert](https://www.cyberciti.biz/faq/bash-test-if-directory-file-exists/)