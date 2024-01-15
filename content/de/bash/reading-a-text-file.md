---
title:                "Eine Textdatei lesen."
html_title:           "Bash: Eine Textdatei lesen."
simple_title:         "Eine Textdatei lesen."
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Wer regelmäßig mit Bash arbeitet, wird früher oder später auf die Notwendigkeit stoßen, Textdateien zu lesen. Sei es zum Auslesen von Konfigurationsdateien oder zum Bearbeiten von Log-Dateien - das Lesen von Textdateien ist ein grundlegender Bestandteil der Bash-Programmierung.

## Wie geht man vor
Das Lesen einer Textdatei in Bash ist relativ einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir die Datei öffnen und in eine Variable speichern. Dies geschieht mit dem Befehl `cat` gefolgt von dem Dateinamen oder Pfad zur Datei. Beispiel:

```Bash
textdatei=$(cat datei.txt)
```

Anschließend können wir auf die Inhalte der Datei zugreifen, indem wir die Variable `$textdatei` verwenden. Zum Beispiel können wir alle Zeilen in der Datei ausgeben lassen:

```Bash
echo "$textdatei"
```

Für eine genauere Kontrolle über den Inhalt der Datei können wir auch Schleifen verwenden, um Zeile für Zeile zu durchlaufen und bestimmte Bedingungen zu erfüllen. Beispiel:

```Bash
while read zeile; do
  if [[ $zeile == *error* ]]; then
    echo "$zeile"
  fi
done <<< "$textdatei"
```

Dieser Code liest jede Zeile der Datei und überprüft, ob das Wort "error" enthalten ist. Wenn ja, wird die Zeile ausgegeben.

## Tiefergehende Informationen
Beim Lesen von Textdateien in Bash gibt es einige Dinge zu beachten. Standardmäßig trennt Bash die einzelnen Zeilen durch einen Zeilenumbruch, was sogenannte "newline characters" sind. Wenn wir jedoch mit Dateien arbeiten, die möglicherweise aus anderen Quellen stammen, z.B. Windows, kann es zu Problemen kommen. Hier kann es hilfreich sein, den Befehl `dos2unix` zu verwenden, der die Zeilenumbrüche entsprechend anpasst.

Außerdem ist zu beachten, dass beim Auslesen von Textdateien auch die Zeilenendungen mitgelesen werden. Das heißt, wenn eine Datei z.B. Tabellenwerte enthält, kann es vorkommen, dass die letzte Spalte in jeder Zeile ein Zeilenumbruch enthält. Dies kann zu unerwarteten Ergebnissen führen und sollte bei der Verarbeitung von Dateien berücksichtigt werden.

## Siehe auch
- [Die offizielle Bash-Dokumentation](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Ein nützlicher Guide zur Arbeit mit Textdateien in Bash](https://www.howtogeek.com/539158/how-to-work-with-text-files-in-the-linux-bash-shell/)
- [Der Befehl `dos2unix` in der Bash-Dokumentation](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)