---
title:                "Bash: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV-Dateien sind eine gängige Art, Daten zu speichern und auszutauschen. Sie werden häufig in Unternehmen, wissenschaftlichen Projekten und sogar in privaten Haushalten verwendet. Als Bash-Programmierer möchten Sie möglicherweise lernen, wie Sie mit CSV-Dateien umgehen und sie in Ihre Skripte integrieren können. In diesem Blog-Beitrag werden wir diskutieren, warum es nützlich ist, mit CSV zu arbeiten und wie Sie dies tun können.

## Wie geht das?

Um mit CSV-Dateien in Bash zu arbeiten, benötigen Sie das "csvtool" -Programm, das Sie mit dem Befehl `apt-get install csvtool` auf Debian-basierten Systemen installieren können. Nach der Installation können Sie "csvtool" verwenden, um mit CSV-Dateien zu arbeiten.

Um beispielsweise eine CSV-Datei zu lesen und auszugeben, können Sie den folgenden Befehl verwenden:

```Bash
csvtool -t ';' -u ' ' cat input.csv
```

Dieser Befehl liest die Datei "input.csv", die Trennzeichen verwendet, und gibt sie mit Leerzeichen als Trennzeichen aus. Sie können auch spezifische Spalten auswählen und die Ausgabe in eine neue Datei schreiben, wie im folgenden Beispiel gezeigt:

```Bash
csvtool -t ';' col 1,3 input.csv > output.csv
```

In diesem Beispiel wird die Ausgabe der ersten und dritten Spalte der Datei "input.csv" in die Datei "output.csv" geschrieben.

## Tiefergehende Einblicke

Nun, da Sie wissen, wie Sie CSV-Dateien in Bash lesen und schreiben können, lassen Sie uns etwas in die Tiefe gehen. Sie können auch mit CSV-Dateien in Schleifen arbeiten und komplexe Bedingungen verwenden, um bestimmte Daten auszuwählen oder zu verarbeiten.

In diesem Beispiel möchten wir alle Zeilen aus einer CSV-Datei ausgeben, die nur positive Zahlen in der ersten Spalte enthält. Dies kann mit dem folgenden Skript erreicht werden:

```Bash
while read line; do
    if [[ "$line" =~ ^[1-9][0-9]*, ]]; then
        echo "$line"
    fi
done < input.csv
```

Dieser Code prüft jede Zeile auf das Muster einer positiven Zahl in der ersten Spalte und gibt nur die Zeilen aus, die diesem Muster entsprechen.

Sie können auch spezifische Zeilen in einer CSV-Datei ändern und die Änderungen in eine neue Datei schreiben. Dies kann mit der Option `-u` und dem Befehl `col` erreicht werden. In diesem Beispiel ändern wir den Inhalt der zweiten Spalte, indem wir jedes Vorkommen von "Ja" durch "Nein" ersetzen und die Ausgabe in eine neue Datei schreiben:

```Bash
csvtool -t ';' -u ' ' col 2,2 input.csv | sed 's/Yes/No/g' > output.csv
```

Dies sind nur einige Beispiele, wie Sie mit CSV-Dateien in Bash umgehen können. Es gibt noch viele weitere Möglichkeiten, je nach Ihren spezifischen Anforderungen und Daten.

## Siehe auch

- Offizielle Dokumentation von csvtool: https://github.com/mbauman/csvtool
- Ein interaktives Tutorial für den Umgang mit CSV-Dateien in Bash: https://www.codementor.io/blog/csv-file-data-parsing-using-bash-practical-tutorial-du1070471
- Ein Stack Overflow-Beitrag mit Tipps und Tricks zum Umgang mit CSV-Dateien in Bash: https://stackoverflow.com/questions/26785478/bash-scripting-how-to-parse-a-csv-file-to-separate-columns