---
title:    "Bash: Die Länge einer Zeichenkette bestimmen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Aufgabe in der Bash-Programmierung. Es ermöglicht uns, die Größe eines Strings zu bestimmen, was nützlich sein kann, um bestimmte Aktionen auszuführen, wie zum Beispiel das Aufteilen eines Strings in einzelne Zeichen oder das Erstellen von Schleifen, die aufgrund der Länge eines Strings ausgeführt werden sollen.

## Wie geht's

Um die Länge eines Strings in Bash zu finden, können wir den Befehl `echo` in Kombination mit der Parametererweiterung `${#string}` verwenden. Schauen wir uns ein Beispiel an:

```Bash
string="Hallo Welt"
echo "${#string}"
```
Dieser Code wird die Ausgabe `11` erzeugen, da der String "Hallo Welt" insgesamt 11 Zeichen hat, einschließlich Leerzeichen. Wir können auch Variablen verwenden, um die Länge eines Strings innerhalb eines Skripts zu speichern oder sie in Bedingungen zu überprüfen:

```Bash
name="Max"
length=${#name}

if [ $length -gt 3 ]
then
    echo "Dein Name hat mehr als 3 Buchstaben."
else
    echo "Dein Name hat 3 oder weniger Buchstaben."
fi
```
In diesem Beispiel wird die Länge des Strings "Max" ermittelt und in der Variable `length` gespeichert. Die Bedingung überprüft nun, ob die Länge größer ist als 3 und gibt je nach Ergebnis entsprechende Nachrichten aus.

## Tiefer Einblick

In der Bash-Programmierung gibt es verschiedene Techniken, um die Länge eines Strings zu ermitteln. Eine davon ist, den Befehl `wc` zusammen mit der Option `-c` zu verwenden, um die Anzahl der Zeichen in einem String zu zählen. Allerdings kann dies zu Problemen führen, wenn der String Leerzeichen oder Sonderzeichen enthält. Daher ist die Verwendung der Parametererweiterung `${#string}` eine sicherere Methode, um die Länge eines Strings zu finden.

Eine weitere wichtige Sache zu beachten ist, dass die Länge eines Strings auch davon abhängen kann, welches Zeichensatz verwendet wird. In der Bash-Programmierung wird standardmäßig der Zeichensatz ASCII verwendet, bei dem jedes Zeichen ein Byte entspricht. Wenn jedoch ein anderer Zeichensatz verwendet wird, wie zum Beispiel UTF-8, kann die Länge eines Strings in Bytes unterschiedlich sein.

## Siehe auch

- [Bash Parametererweiterungen](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Übersicht über ASCII-Codes](https://ascii.cl/)