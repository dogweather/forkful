---
title:    "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

In vielen Programmiersprachen ist es eine gängige Praxis, zu prüfen, ob ein Verzeichnis existiert, bevor es verwendet wird. Dies gilt besonders für Skriptsprachen wie die Fish Shell, die häufig zur Automatisierung von Aufgaben verwendet wird. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie man in Fish Shell überprüfen kann, ob ein Verzeichnis vorhanden ist.

## Wie

Um zu überprüfen, ob ein Verzeichnis existiert, können wir das `test` -Kommando in Kombination mit der `-d`-Option verwenden. In der Fish Shell sieht der Code dafür folgendermaßen aus:

```Fish Shell
test -d /Pfad/zum/Verzeichnis
```

Wenn das Verzeichnis existiert, gibt das `test`-Kommando einen Exit-Code 0 zurück, andernfalls einen Exit-Code 1. Dies können wir verwenden, um bedingte Anweisungen in unserem Code auszuführen. Hier ist ein Beispiel, das eine Nachricht ausgibt, wenn das Verzeichnis existiert:

```Fish Shell
if test -d /Pfad/zum/Verzeichnis
  echo "Das Verzeichnis existiert."
end
```

## Deep Dive

Bevor wir ein Verzeichnis überprüfen, ist es wichtig zu verstehen, dass existierende und lesbare Verzeichnisse nicht dasselbe sind. Ein Verzeichnis kann existieren, aber aufgrund von Berechtigungen nicht lesbar sein. Um sicherzustellen, dass das Verzeichnis nicht nur existiert, sondern auch lesbar ist, können wir auch die `-r`-Option verwenden.

Eine weitere Möglichkeit, ein Verzeichnis zu überprüfen, besteht darin, die Fish Shell-eigene Funktion `is_dir` zu verwenden. Diese Funktion liefert einen booleschen Wert zurück und kann auch mit absoluten oder relativen Pfaden verwendet werden.

## Siehe auch

- [Fish Shell-Testkommando](https://fishshell.com/docs/current/commands.html#test)
- [Fish Shell-is_dir Funktion](https://fishshell.com/docs/current/cmds/is_dir.html)
- [Check if Directory exists in Fish Shell](https://unix.stackexchange.com/questions/346525/check-if-directory-exists-in-fish-shell)