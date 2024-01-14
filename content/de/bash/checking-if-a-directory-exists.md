---
title:    "Bash: Überprüfen, ob ein Verzeichnis vorhanden ist"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Warum

In der Shell-Programmierung ist es oft notwendig zu überprüfen, ob ein bestimmtes Verzeichnis existiert, bevor bestimmte Befehle ausgeführt werden. Dies kann den Rechner vor schwerwiegenden Fehlern schützen und bessere Fehlerbehandlung ermöglichen.

## Wie

Es gibt verschiedene Möglichkeiten, um in Bash zu überprüfen, ob ein Verzeichnis existiert. Eine Möglichkeit ist die Verwendung des Befehls `test`, der häufig in Shell-Skripten verwendet wird. Der Befehl `test` gibt einen Wert von 0 zurück, wenn das angegebene Verzeichnis existiert, und einen Wert ungleich 0, wenn es nicht existiert. Hier ist ein Beispielcode:

```Bash
if test -d meine_verzeichnis
then
    echo "Das Verzeichnis existiert!"
else
    echo "Das Verzeichnis existiert nicht!"
fi
```

Eine weitere Möglichkeit ist die Verwendung des Befehls `test` in Verbindung mit der Bedingung `[[ -d ]]`, die nur `true` zurückgibt, wenn das Verzeichnis existiert. Hier ist ein weiteres Beispiel:

```Bash
if [[ -d meine_verzeichnis ]]
then
    echo "Das Verzeichnis existiert!"
else
    echo "Das Verzeichnis existiert nicht!"
fi
```

## Tiefentauchen

Um tiefer in das Thema einzutauchen, ist es wichtig zu verstehen, dass der Befehl `test` nicht nur zum Überprüfen von Verzeichnissen verwendet werden kann, sondern auch zum Überprüfen von Dateien oder anderen Bedingungen. Es gibt auch verschiedene Flags, die dem Befehl `test` hinzugefügt werden können, um bestimmte Bedingungen zu überprüfen. Eine vollständige Liste finden Sie in der Bash-Dokumentation.

Ein weiterer wichtiger Punkt ist, dass die Bedingung `[[ -d ]]` auf Shell-Umgebungsvariablen und Argumente aufgerufen werden kann, während `test` immer mit dem vollständigen Dateipfad aufgerufen werden muss.

## Siehe auch

- [Bash-Dokumentation] (https://www.gnu.org/software/bash/manual/bash.html)
- [Einführung in die Shell-Programmierung] (https://www.shellscript.sh/)