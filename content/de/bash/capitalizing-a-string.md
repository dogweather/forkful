---
title:    "Bash: Großschreibung einer Zeichenkette"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum
Wenn Sie sich jemals gefragt haben, wie Sie Großbuchstaben in einem Bash-Skript verwenden können, dann sind Sie hier richtig! Hier werden wir besprechen, warum es wichtig sein könnte, eine Zeichenfolge zu capitalisieren und wie man es richtig macht.

## Wie geht das?
Das Capitalizing von Strings in Bash ist eine einfache Aufgabe, die mit dem `tr` Befehl durchgeführt werden kann. Der `tr` Befehl steht für "Translate" und kann verwendet werden, um Zeichen in einer Zeichenfolge zu ändern. Hier folgt ein Beispiel, wie man eine Zeichenfolge in Großbuchstaben übersetzt:

```Bash
# Erstelle eine Variable mit einer Zeichenfolge
string="hallo welt"
 
# Verwende den `tr` Befehl, um die Zeichenfolge in Großbuchstaben zu übersetzen
string_caps=$(echo $string |tr '[:lower:]' '[:upper:]')

# Gib das Ergebnis aus
echo $string_caps # Output: HALLO WELT
```

Hier haben wir zunächst eine Variable mit der Zeichenfolge "hallo welt" erstellt. Dann wurde der `tr` Befehl verwendet, um alle Kleinbuchstaben in Großbuchstaben zu übersetzen. Schließlich wurde das Ergebnis mit dem `echo` Befehl ausgegeben.

## Deep Dive
Der `tr` Befehl verwendet eine spezielle Syntax, um Zeichen in einer Zeichenfolge zu übersetzen. Hier ist ein Überblick über diese Syntax:

1. Der `tr` Befehl wird immer mit zwei Zeichensätzen verwendet. Der erste Satz ist der Satz, der übersetzt werden soll, und der zweite Satz ist der Satz, in den übersetzt werden soll.
2. Der Befehl wird in der Form `tr '[zeichensatz1]' '[zeichensatz2]'` verwendet.
3. Die einzelnen Zeichen in jedem Satz werden mit Doppelpunkten getrennt.
4. Wenn ein Bindestrich (-) verwendet wird, bedeutet dies, dass alle Zeichen zwischen den angegebenen Zeichen übersetzt werden sollen.

Eine vollständige Liste der verfügbaren Zeichensätze und deren Verwendung finden Sie in der [manpage](https://man7.org/linux/man-pages/man1/tr.1.html) zur `tr` Befehl.

## Siehe auch
- [The `tr` Befehl in der Bash-Referenz](https://www.gnu.org/software/bash/manual/html_node/The-tr-Builtin.html)
- [Eine Einführung in die Bash-Programmierung](https://www.tutorialspoint.com/unix/unix-shell.htm)
- [Eine Übersicht über Linux-Befehle](https://www.linux.com/training-tutorials/linux-commands-beginners/)

Danke fürs Lesen und viel Spaß beim Capitalizing Ihrer Strings mit Bash!