---
title:                "Bash: Eine Datum in einen String umwandeln"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Datums in einen String ist eine häufige Aufgabe in der Bash-Programmierung. Es kann hilfreich sein, um Daten in einem benutzerfreundlichen Format auszugeben oder um Werte in einer Datei zu speichern. In diesem Blog-Beitrag werde ich erläutern, wie man ein Datum in einen String umwandelt und warum es in der Bash-Programmierung nützlich sein kann.

## Wie geht's?

Um ein Datum in einen String umzuwandeln, können wir die `date`-Funktion in Bash verwenden. Schauen wir uns ein Beispiel an:

```Bash
# Setzen eines Datums
date_to_convert='01/01/2020'

# Konvertieren des Datums in den String
converted_date=$(date -d "$date_to_convert" + "%d.%m.%Y")

# Ausgabe des Strings
echo "$converted_date"
```

Im obigen Code wird zunächst das Datum `01/01/2020` in der Variable `date_to_convert` gespeichert. Dann wird die `date`-Funktion mit dem Parameter `-d` verwendet, um das Datum in das gewünschte Format umzuwandeln. In diesem Fall wird das Datum im Format `TT.MM.JJJJ` in der Variablen `converted_date` gespeichert. Schließlich wird der String mit der `echo`-Funktion ausgegeben.

Die Verwendung des Parameters `-d` ist jedoch nicht zwingend erforderlich. Wenn wir das Datum ohne diesen Parameter angeben, wird das aktuelle Datum in das gewünschte Format umgewandelt.

```Bash
# Konvertieren des aktuellen Datums in den String
converted_date=$(date + "%d.%m.%Y")

# Ausgabe des Strings
echo "$converted_date"
```

In beiden Beispielen sehen wir, dass das Datum in den String `01.01.2020` umgewandelt wurde.

## Tiefentauchen

Wenn wir uns die `date`-Funktion genauer ansehen, können wir verschiedene Optionen für das Format des Strings festlegen. Zum Beispiel können wir das Datum im ISO-Format `JJJJ-MM-TT` umwandeln oder sogar Stunden, Minuten und Sekunden hinzufügen.

```Bash
# Konvertieren des Datums im ISO-Format mit Zeitangabe
converted_date=$(date + "%Y-%m-%d %H:%M:%S")

# Ausgabe des Strings
echo "$converted_date"
```

Die vollständige Dokumentation der `date`-Funktion und ihrer Optionen kann in der Bash-Shell mit `man date` abgerufen werden.

## Siehe auch

- [Date Manipulation in Bash (Englisch)](https://opensource.com/article/18/12/bash-tricks)
- [Bash Scripting Tutorial (Deutsch)](https://www.shellscript.sh/index.html)