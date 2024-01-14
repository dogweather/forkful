---
title:                "Fish Shell: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

Das Arbeiten mit CSV-Dateien ist eine häufige Aufgabe im Bereich der Datenverarbeitung. Mit dem Fish Shell können Sie diesen Prozess effizient und unkompliziert durchführen.

## Wie man mit CSVs im Fish Shell arbeitet

Das Fish Shell bietet verschiedene Möglichkeiten, um CSV-Dateien zu lesen, zu schreiben und zu verarbeiten. Hier sind einige Beispiele, wie Sie das in Ihrem eigenen Code nutzen können:

### CSV-Datei lesen

Um eine CSV-Datei zu lesen, verwenden Sie den Befehl `cat` mit einer Kombination aus Redirection und dem Befehl `string split`:

```
cat datei.csv | string split "," | echo (string split "\n" $array)
```

Im obigen Beispiel lesen wir die Datei "datei.csv" mit dem Befehl `cat` und teilen sie mit dem Trennzeichen "," auf. Dann verwenden wir `string split` erneut, um die einzelnen Zeilen in ein Array zu speichern. Schließlich geben wir mit dem Befehl `echo` das Array aus.

### CSV-Datei schreiben

Um Daten in eine CSV-Datei zu schreiben, können Sie den Befehl `set` in Kombination mit `string join` verwenden:

```
set -l array "1,2,3,4,5"
string join "\n" $array > datei.csv
```

Hier erstellen wir zuerst ein Array mit den Werten "1,2,3,4,5". Dann verwenden wir `string join` und das Trennzeichen "\n", um das Array formatiert in die CSV-Datei "datei.csv" zu schreiben.

### CSV-Daten verarbeiten

Das Fish Shell bietet auch Funktionen, um Daten aus einer CSV-Datei zu filtern und zu bearbeiten. Zum Beispiel können Sie mit dem Befehl `contains` nach bestimmten Werten in einer CSV-Datei suchen:

```
cat datei.csv | while read line
	if contains $line "Wert"
		echo $line
	end
end
```

Mit diesem Code durchsuchen wir die Datei "datei.csv" nach Zeilen, die den Wert "Wert" enthalten, und geben diese aus.

## Tiefer Einblick

Das Fish Shell bietet eine Vielzahl von Funktionen und Möglichkeiten, um mit CSV-Dateien zu arbeiten. Ein guter Einstiegspunkt ist die Dokumentation der offiziellen Website: https://fishshell.com/docs/current/index.html. Hier finden Sie eine detaillierte Beschreibung aller verfügbaren Befehle und Funktionen zum Lesen, Schreiben und Verarbeiten von CSV-Dateien.

## Siehe auch

- https://fishshell.com/docs/current/index.html
- https://fishshell.com/docs/current/cmds.html
- https://fishshell.com/docs/current/functions.html