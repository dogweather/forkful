---
title:                "Das Abrufen des aktuellen Datums"
html_title:           "Fish Shell: Das Abrufen des aktuellen Datums"
simple_title:         "Das Abrufen des aktuellen Datums"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Abrufen des aktuellen Datums ist eine gängige Aufgabe für Programmierer. Es ermöglicht ihnen, das aktuelle Datum und die Uhrzeit in ihren Programmen zu verwenden. Dies kann nützlich sein, um Zeitstempel zu generieren, auf Datenbanken zuzugreifen oder einfach nur, um zu überprüfen, ob eine bestimmte Aufgabe zu einem bestimmten Zeitpunkt ausgeführt werden soll.

# Wie geht das?
Das Abrufen des aktuellen Datums in der Fish Shell ist ganz einfach. Es kann mit dem Befehl "date" und der Option "+%d/%m/%Y" durchgeführt werden. Dies gibt das aktuelle Datum im Format Tag/Monat/Jahr aus.

```Fish Shell
date +%d/%m/%Y
```

Das Ergebnis sieht dann beispielsweise so aus: 17/04/2021.

# Tiefere Einblicke
Das Abrufen des aktuellen Datums ist eine weit verbreitete Aufgabe, die in fast jeder Programmiersprache und Shell möglich ist. Es ist auch wichtig, um sicherzustellen, dass Programme zuverlässig auf das aktuelle Datum zugreifen können.

Eine Alternative zum Befehl "date" in der Fish Shell ist der Befehl "gdate". Dies ist ein in der GNU Core Utilities enthaltener Befehl, der erweiterte Funktionen für das Abrufen des Datums und der Uhrzeit bietet.

Die Implementierung des aktuellen Datums in der Fish Shell nutzt das Unix Epoch System, das die Anzahl der Sekunden seit dem 1. Januar 1970 um 00:00 Uhr UTC zählt. Dieser Wert wird dann in ein menschenlesbares Datum und eine Zeit umgewandelt.

# Siehe auch
- Offizielle Fish Shell Dokumentation zum Befehl "date": https://fishshell.com/docs/current/cmds/date.html
- GNU Core Utilities: https://www.gnu.org/software/coreutils/coreutils.html