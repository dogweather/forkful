---
title:                "Arbeiten mit CSV"
html_title:           "Bash: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum CSV?

CSV (Comma-Separated Values) ist ein gängiges Dateiformat, das häufig für die Speicherung und den Austausch von tabellarischen Daten verwendet wird. Es ist sehr einfach zu lesen und zu schreiben, sowohl für Menschen als auch für Computer, wodurch es zu einem beliebten Format in der Programmierung wird.

## Wie man mit CSV arbeitet

Um mit CSV-Dateien in Bash zu arbeiten, benötigt man grundlegende Kenntnisse über die Befehle "cut", "head", "tail" und "grep". Hier sind einige Beispiele dafür, wie man diese Befehle verwendet, um mit CSV-Dateien zu arbeiten:

```Bash
# CSV-Datei in drei Spalten aufteilen und nur die erste Spalte ausgeben
cut -d"," -f1 eingabe.csv

# Erste Zeilen einer CSV-Datei ausgeben
head -n 5 eingabe.csv

# Letzten Zeilen einer CSV-Datei ausgeben
tail -n 5 eingabe.csv

# Zeilen mit einer bestimmten Zeichenkette in der CSV-Datei finden
grep "Keyword" eingabe.csv
```

Hier ist eine Beispiel-CSV-Datei, die wir für diese Coding-Beispiele verwenden werden:

```Bash
Name,Alter,Stadt
Anna,25,Berlin
Max,32,München
Lisa,28,Hamburg
```

Nun, da wir wissen, wie wir mit den grundlegenden Befehlen arbeiten können, können wir tiefer in die Arbeit mit CSV-Dateien eintauchen.

## Tiefer eintauchen

### Zeichenfolgen in CSV-Dateien formatieren

Standardmäßig werden CSV-Dateien in Bash wie jede andere Textdatei behandelt, was bedeutet, dass jede Zeile einfach als eine lange Zeichenfolge angezeigt wird. Um die Lesbarkeit zu verbessern, können wir die Befehle "echo" und "printf" verwenden, um die Zeichenfolgen in einer CSV-Datei formatiert auszugeben.

```Bash
# Zeilen in einer CSV-Datei formatiert ausgeben
echo "Name     Alter    Stadt"
printf "%-10s %-7s %-7s \n" Anna 25 Berlin
```

Dies würde die folgende Ausgabe erzeugen:

```Bash
Name     Alter    Stadt
Anna     25       Berlin
```

### CSV-Dateien zusammenführen

Manchmal haben wir mehrere separate CSV-Dateien und müssen sie zu einer einzigen Datei zusammenführen. Dafür können wir den Befehl "cat" verwenden, der die Dateien zeilenweise zusammenfügt. Aber in CSV-Dateien gibt es normalerweise eine Header-Zeile, die nicht dupliziert werden sollte. Daher müssen wir diese Zeile aus der zweiten Datei mit dem Befehl "tail" entfernen, bevor wir die Dateien zusammenführen.

```Bash
# CSV-Dateien zusammenführen
cat eingabe1.csv eingabe2.csv > ausgabe.csv
tail -n +2 eingabe2.csv >> ausgabe.csv
```

### CSV-Dateien bearbeiten und speichern

Um eine CSV-Datei zu bearbeiten und die Änderungen zu speichern, müssen wir die Datei in eine Variable laden, die bearbeitet werden kann, und dann die Variable in eine neue Datei speichern.

```Bash
# CSV-Datei in eine Variable laden
csv="$(cat eingabe.csv)"

# Eine Zeile in der Variable bearbeiten
csv="${csv//$Keyword/$New_keyword}"

# Variable in eine neue Datei speichern
echo "$csv" > ausgabe.csv
```

Jetzt können wir die bearbeitete CSV-Datei "ausgabe.csv" verwenden.

## Siehe auch

- [Bash Kurzanleitung] (https://dev.to/awwsmm/bash-commands-hello-world-examples-4nk9)
- [Bash for Beginners: Handling CSV Files] (https://opensource.com/article/18/5/you-dont-know-bash-intro-bash-csvs)
- [Offizielle Bash-Dokumentation] (https://www.gnu.org/software/bash/manual/bash.html)