---
title:                "Das aktuelle Datum erhalten"
html_title:           "Bash: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die aktuelle Datum und Uhrzeit zu erhalten, bedeutet, dass der Programmierer den aktuellen Kalender und die aktuelle Zeit auf dem Computer abruft und diese Informationen in seinem Code verwenden kann. Programmierer tun dies, um ihre Anwendungen aktuell und zeitbezogen zu halten.

## Wie?

Um das aktuelle Datum und die Uhrzeit in Bash zu erhalten, verwende das Befehlszeilentool "date". Hier ist ein Beispielcode, der das aktuelle Datum und die Uhrzeit im Format "Jahr-Monat-Tag Stunde:Minute:Sekunde" ausgibt:

```Bash
date +"%Y-%m-%d %H:%M:%S"
```

Der Output dieses Codes könnte zum Beispiel so aussehen:

```Bash
2019-06-27 14:30:22
```

Um das aktuelle Datum und die Uhrzeit in einer anderen Zeitzone auszugeben, kannst du den Befehl "TZ" verwenden und die gewünschte Zeitzone als Argument angeben:

```Bash
TZ=America/New_York date +"%Y-%m-%d %H:%M:%S"
```

Der Output dieses Codes zeigt die aktuelle Datum und Uhrzeit in New York an, also eine Stunde vor der vorherigen Ausgabe:

```Bash
2019-06-27 13:30:22
```

## Gründliche Analyse

Das Abrufen des aktuellen Datums und der Uhrzeit ist besonders hilfreich, um Aufgaben zu automatisieren, die zu bestimmten Zeitpunkten ausgeführt werden sollen. Es ist auch nützlich für die Protokollierung von Ereignissen innerhalb einer Anwendung. Alternativ können Programmierer auch eine integrierte Funktion wie "time()" verwenden, um die aktuelle Zeit in Millisekunden abzurufen. Die Implementierung der "date"-Funktion in Bash stammt aus dem GNU Date-Paket, welches Teil der Coreutils-Software-Sammlung ist.

## Siehe auch

- [Dokumentation der "date"-Funktion in Bash](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Andere Möglichkeiten, die aktuelle Zeit in Bash zu erhalten](https://askubuntu.com/questions/102892/how-do-i-find-the-time-difference-between-today-and-it-a-reference-date-prompt)
- [Der Coreutils-Sammlung und dem GNU Date-Paket](https://www.gnu.org/software/coreutils/index.html)