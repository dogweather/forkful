---
title:                "Bash: Umwandlung eines Datums in einen String"
simple_title:         "Umwandlung eines Datums in einen String"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Sie fragen sich vielleicht, warum es wichtig ist, ein Datum in einen String umzuwandeln. Nun, in der Programmierung gibt es oft verschiedene Formate, in denen ein Datum angezeigt werden kann. Durch die Umwandlung in einen String können Sie sicherstellen, dass das Datum in einem einheitlichen Format angezeigt wird, unabhängig davon, wie es in der Datenbank oder im Code gespeichert ist.

## Wie geht's

Die Umwandlung eines Datums in einen String in Bash ist relativ einfach. Folgen Sie einfach den Schritten unten mit den dazugehörigen Code-Beispielen:

1. Definieren Sie das Datum im gewünschten Format
```Bash
date="2021/12/31"
```

2. Verwenden Sie den `date` Befehl und das gewünschte Datumsformat als Option.
```Bash
str_date=$(date +"%d.%m.%Y" -d "$date")
```

3. Geben Sie den Inhalt der Variable `str_date` aus
```Bash
echo $str_date
# Output: 31.12.2021
```

Durch die Verwendung der `date` Funktion können Sie das Datum in verschiedenen Formaten anzeigen lassen, indem Sie die Formatierungsoption entsprechend anpassen.

## Tiefere Einblicke

In Bash gibt es verschiedene Möglichkeiten, ein Datum in einen String umzuwandeln. Neben dem verwendeten Beispiel können Sie auch den `printf` Befehl verwenden, um das Datum in anderen Formaten auszugeben. Sie können auch mit der `awk` Funktion arbeiten, um das Datum auszulesen und in einen String zu konvertieren.

Es ist auch wichtig zu beachten, dass die Umgebungseinstellungen wie die Zeitzoneneinstellung und das verwendete Format der Ausgabe beeinflussen können. Deshalb ist es wichtig, sicherzustellen, dass diese Einstellungen richtig konfiguriert sind, um unerwartete Ergebnisse zu vermeiden.

## Siehe auch

- [Bash-Referenzhandbuch: Datums- und Zeitfunktionen](https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html#Shell-Functions)
- [Bash-Hackers Wiki: Daten- und Uhrzeitfunktionen](https://wiki.bash-hackers.org/commands/builtin/date)
- [Linuxize: Wie man ein Datum in Bash formatiert](https://linuxize.com/post/how-to-format-date-in-bash/)