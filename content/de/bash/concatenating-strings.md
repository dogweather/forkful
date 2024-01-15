---
title:                "Strings verbinden"
html_title:           "Bash: Strings verbinden"
simple_title:         "Strings verbinden"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Bash-Skripten ist eine schnelle und einfache Möglichkeit, wiederkehrende Aufgaben automatisiert auszuführen. In vielen Fällen ist es nötig, Textzeilen miteinander zu verknüpfen, um komplexe Befehle zu erstellen. Das sogenannte "String Concatenation" ist eine wichtige Fähigkeit in der Bash-Programmierung und kann dir dabei helfen, deine Arbeitsprozesse zu verbessern.

## Wie geht man vor?

Das Verknüpfen von Strings in Bash ist ganz einfach. Hier ein Beispiel:

```Bash
first_name="Max"
last_name="Mustermann"

echo "Hallo, mein Name ist $first_name $last_name."
```

Dieses Skript wird folgende Ausgabe erzeugen:

`Hallo, mein Name ist Max Mustermann.`

Um Strings zu verknüpfen, nutzen wir den Operator `=` und geben den zu verknüpfenden Text in Anführungszeichen an. Dabei können auch Variablen eingefügt werden, indem wir den Variablennamen mit einem Dollarzeichen `$` markieren.

Alternativ können wir auch den Befehl `printf` nutzen, der es erlaubt, mehrere Variablen und Textzeichen in einer Zeile auszugeben:

```Bash
first_name="Max"
last_name="Mustermann"

printf "Mein Name ist %s %s." $first_name $last_name
```

Hier wird das Ergebnis dasselbe sein wie im ersten Beispiel. Der Befehl `printf` erlaubt es jedoch, die Reihenfolge der Variablen zu definieren und zusätzlich Textformatierung anzuwenden.

## Tiefer gehen

In Bash gibt es verschiedene Wege, um Strings zu verknüpfen. Eine weitere Möglichkeit ist die Verwendung von Backticks `` ` ``, um Ergebnisse von Befehlen in Strings einzubinden.

```Bash
directory=`pwd`
filename="my_file.txt"
file_path="$directory/$filename"

echo $file_path
```

Dieses Skript gibt den vollständigen Pfad zur Datei `my_file.txt` aus, indem es `pwd` verwendet, um den aktuellen Pfad zu ermitteln und ihn mit dem Dateinamen zu verknüpfen.

Außerdem können wir auch Zeichenketten mit dem Operator `+=` aneinanderhängen:

```Bash
greeting="Hallo, "
greeting+="wie geht es dir?"

echo $greeting
```

Die Ausgabe wird `Hallo, wie geht es dir?` sein.

Es gibt noch viele weitere Möglichkeiten, um Strings in Bash zu verknüpfen. Indem du dich intensiver mit der Sprache auseinandersetzt, wirst du immer neue und kreative Lösungen finden.

## Siehe auch

- [Bash Scripting Tutorial auf Deutsch](https://linuxacademy.com/blog/linux/bash-scripting-tutorial/)
- [Bash Beginner's Guide](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)