---
title:                "Bash: Das aktuelle Datum erhalten"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum zu erhalten ist in der Bash-Programmierung eine wichtige Fähigkeit, die bei der Automatisierung von Aufgaben hilft. Egal ob für die Erstellung von Dateinamen oder das Aufzeichnen von Logs, die aktuelle Datum- und Uhrzeitangabe ist oft unerlässlich.

## Wie geht man vor?

Es gibt mehrere Möglichkeiten, das aktuelle Datum in Bash zu erhalten. Eine einfache Methode ist die Verwendung des `date`-Befehls. Dieser kann auf verschiedene Arten formatiert werden, um die Ausgabe an die eigenen Bedürfnisse anzupassen. Hier sind einige Beispiele:

```Bash
# Aktuelles Datum und Uhrzeit im Format "Tag Monat Jahr Stunden:Minuten:Sekunden"
echo $(date +"%d %b %Y %H:%M:%S") 
# Ausgabe: 07 Jul 2021 16:55:23

# Nur das Jahr und der Monat
echo $(date +"%Y%m") 
# Ausgabe: 202107

# Nur die Uhrzeit im 24-Stunden-Format
echo $(date +"%H:%M") 
# Ausgabe: 16:55
```

Weitere Optionen und Möglichkeiten sind in der manuellen Seite des `date`-Befehls zu finden.

## Tiefergehende Informationen

Das `date`-Kommando greift auf die Systemzeit des Computers zu, um das aktuelle Datum zu erhalten. Diese Zeit ist üblicherweise auf die Koordinierte Weltzeit (UTC) eingestellt, jedoch können verschiedene Zeitzonen und Sommer- und Winterzeitangaben in der Ausgabe berücksichtigt werden.

Eine alternative Methode, das aktuelle Datum zu ermitteln, ist die Verwendung von Variablen, die auf die interne Bash-Uhr zugreifen. Diese rechnet die vergangenen Sekunden seit dem 1. Januar 1970 in Coordinated Universal Time (UTC) um. Ein Beispiel für eine solche Variable ist `SECONDS`, die seit dem Start des aktuellen Bash-Prozesses vergangene Sekunden zählt.

## Siehe auch

- [Manuelle Seite des `date`-Befehls](https://linux.die.net/man/1/date)
- [Bash-Uhr und Variablen](https://www.thegeekstuff.com/2010/08/bash-shell-builtin-variables-how-to-use-them/)
- [Bash-Befehle zum Ermitteln des aktuellen Datums](https://www.cyberciti.biz/faq/how-to-format-date-for-display-or-use-in-a-shell-script/)