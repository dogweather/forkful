---
title:                "Bash: Die Länge eines Strings finden"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist eine häufige Aufgabe beim Programmieren. Es kann verwendet werden, um sicherzustellen, dass die Eingabe nicht zu lang ist oder um den benötigten Speicherplatz einer Variablen zu bestimmen. In diesem Blogbeitrag erfahren Sie, wie Sie die Länge einer Zeichenkette in Bash finden und warum es eine nützliche Fähigkeit ist.

## Wie Geht Es

Um die Länge einer Zeichenkette in Bash zu finden, können Sie den Befehl "echo" in Verbindung mit dem Parameter "-n" verwenden. Zum Beispiel:

```Bash
echo -n "Hallo Welt"
```
Die Ausgabe dieses Befehls wird die Anzahl der Zeichen in der Zeichenkette "Hallo Welt" anzeigen, in diesem Fall 11. Der Parameter "-n" sorgt dafür, dass keine neue Zeile am Ende der Ausgabe hinzugefügt wird.

Eine andere Möglichkeit, die Länge einer Zeichenkette zu finden, ist die Verwendung des Befehls "expr" in Kombination mit der Funktion "length". Zum Beispiel:

```Bash
expr length "Hallo Welt"
```
Die Ausgabe dieses Befehls wird ebenfalls die Länge der Zeichenkette "Hallo Welt" anzeigen.

## Tiefergehende Informationen

In Bash ist jede Zeichenkette eine Liste von Nullen und Einsen, die als ASCII-Codes dargestellt werden. Die Länge einer Zeichenkette ist daher die Anzahl der Zeichen in dieser Liste.

Eine wichtige Sache zu beachten ist, dass Leerzeichen ebenfalls als Zeichen gezählt werden. Daher wird die Länge einer Zeichenkette, die nur aus Leerzeichen besteht, auch als positiver Wert angezeigt.

## Siehe Auch

- [Bash-Zeichenketten - Manipulation und Vergleiche](https://www.linuxwiki.de/Bash-Zeichenketten_-_Manipulation_und_Vergleiche)
- [Häufige Bash-Befehle](https://wiki.archlinux.org/index.php/Category:Command_line_tools_(Deutsch))
- [Offizielle Bash-Dokumentation](https://www.gnu.org/software/bash/manual/bash.html)