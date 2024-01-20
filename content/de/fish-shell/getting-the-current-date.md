---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mit Fish Shell das aktuelle Datum bekommen

## Was & Warum?

Das aktuelle Datum zu ermitteln ist recht einfach: Es ist der Prozess, durch den wir den aktuellen Tag, Monat und Jahr von unserem System erhalten. Dies ist für Programmierer aus vielen Gründen wichtig, z.B. für Protokollierung, Zeitstempel und die Durchführung zeitbasierter Aufgaben.

## So geht's:

Um das aktuelle Datum in Fish Shell (derzeitige Version) zu bekommen, verwenden wir den `date` Befehl:

```Fish Shell
date
```

Der obige Befehl zeigt uns das aktuelle Datum und die Zeit an:

```Fish Shell
Thu Mar 11 17:17:34 UTC 2021
```

Wenn Sie nur das Datum sehen möchten, können Sie die Ausgabe formatieren:

```Fish Shell
date "+%Y-%m-%d"
```
was folgendes ausgibt:

```Fish Shell
2021-03-11
```

## Deep Dive

Fish Shell ist eine benutzerfreundliche Befehlszeilenschnittstelle, die 2005 eingeführt wurde. Tatsächlich ist der `date` Befehl viel älter und war bereits in Unix-Systemen der 70er Jahre verfügbar, was seine weit verbreitete Verwendung und Akzeptanz erklärt.

Es gibt auch Alternativen zum `date` Befehl, wie `strftime` in Perl oder `datetime` in Python, die ähnliche Funktionalitäten bieten, abhängig von den spezifischen Anforderungen.

Im Falle des `date` Befehls ruft das Betriebssystem intern eine Systemuhr auf, um das aktuelle Datum und die Uhrzeit zu ermitteln und auf dem Bildschirm anzuzeigen.

## Siehe auch

Hier finden Sie weitere Informationen zur Datums- und Zeitverarbeitung in Fish Shell und alternativen Sprachen:

- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- Perl strftime: https://perldoc.perl.org/functions/strftime.html
- Python datetime: https://docs.python.org/3/library/datetime.html