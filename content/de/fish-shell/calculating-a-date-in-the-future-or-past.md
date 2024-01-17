---
title:                "Eine Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "Fish Shell: Eine Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Eine Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Was & Warum?

Die Berechnung eines Datums in der Zukunft oder Vergangenheit ist eine häufige Aufgabe für Programmierer. Dies kann nützlich sein, um zukünftige Termine zu planen oder vergangene Ereignisse zu überprüfen. Oftmals basieren diese Berechnungen auf vorhandenen Daten, wie zum Beispiel der aktuellen Systemzeit oder einem bestimmten Datum.

# Wie geht's?

Die Fish Shell bietet eine praktische Möglichkeit, Datumsberechnungen direkt in der Kommandozeile durchzuführen. Dazu können verschiedene Shell-Befehle wie `date` oder `strftime` verwendet werden. Sehen wir uns einige Beispiele an:

```
Fish Shell 3.1.2
Copyright (c) 2011-2019 Per Arneng. Copyright (c) 2010-2019 David Adam.
fish is fully POSIX compliant.
- fish_shell_rc
date -d "next year" +"%A, %B %d, %Y"
Mon, January 03, 2022

date -d "-5 days" +"%A, %B %d, %Y"
Tuesday, November 02, 2021
```

Dieser Code gibt das Datum der nächsten Jahresfeier sowie das Datum vor 5 Tagen aus. Mit dem `date` Befehl können auch Stunden, Minuten oder andere Zeitangaben hinzugefügt werden, um präzisere Berechnungen durchzuführen.

# Tiefentauchen

Die Berechnung von Datum und Zeit in der Shell ist eine praktische Alternative zu externen Programmen wie `cal` oder `dateutils`. Mit der `date` Funktion können komplexe Berechnungen durchgeführt werden, wie zum Beispiel das Ermitteln des Wochentags eines bestimmten Datums oder das Hinzufügen von Monaten zu einem vorhandenen Datum.

# Siehe auch

Weitere Informationen zur Verwendung von Datumsfunktionen in der Fish Shell finden Sie in der offiziellen Dokumentation unter [https://fishshell.com/docs/current/cmds/date.html](https://fishshell.com/docs/current/cmds/date.html). Für alternative Ansätze gibt es auch das Programm `dateutils` unter [https://www.fresse.org/dateutils/](https://www.fresse.org/dateutils/).