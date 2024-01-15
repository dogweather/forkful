---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Bash: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Es gibt verschiedene Gründe, warum jemand prüfen möchte, ob ein Verzeichnis in Bash existiert. Möglicherweise müssen Sie sicherstellen, dass Ihr Skript nur in vorhandenen Verzeichnissen ausgeführt wird, oder Sie möchten eine spezifische Aktion ausführen, wenn ein Verzeichnis nicht existiert.

## Wie funktioniert es?

Um zu überprüfen, ob ein bestimmtes Verzeichnis existiert, können Sie den Befehl "test" oder dessen übliche Kurzform "-d" verwenden, gefolgt von dem gewünschten Verzeichnisnamen. Zum Beispiel:

```Bash
if [ -d "mein_verzeichnis" ]; then
  echo "Das Verzeichnis existiert."
else
  echo "Das Verzeichnis existiert nicht."
fi
```

In diesem Beispiel wird der Befehl "test -d" verwendet, um das Verzeichnis "mein_verzeichnis" zu überprüfen. Wenn es existiert, wird die erste Meldung ausgegeben, andernfalls die zweite.

Ein weiterer nützlicher Befehl ist "mkdir -p", der ein Verzeichnis erstellt, wenn es noch nicht existiert. Sie können also zuerst überprüfen, ob ein Verzeichnis existiert, und falls nicht, es mit "mkdir -p" erstellen.

## Tiefgehende Informationen

Es gibt verschiedene andere Methoden, um zu überprüfen, ob ein Verzeichnis existiert, wie z.B. die Verwendung von "ls" in Kombination mit "grep" oder das Erstellen eigener Funktionen. Es ist jedoch zu beachten, dass "test -d" die effizienteste und standardmäßige Art ist, dies in Bash zu tun.

## Siehe auch

- [Dokumentation zu "test" and "-d" von Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Weitere nützliche Befehle in Bash](https://www.tldp.org/LDP/abs/html/)