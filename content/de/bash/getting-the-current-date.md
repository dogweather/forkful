---
title:    "Bash: Das aktuelle Datum erhalten"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist in vielen Bash-Programmen ein sehr nützliches Feature. Es ermöglicht die Dynamik von Skripten und die Verarbeitung von datumsbezogenen Daten.

## Wie

Um das aktuelle Datum in Bash abzurufen, können wir den Befehl `date` verwenden. Dieser Befehl gibt das aktuelle Datum und die Uhrzeit im gewünschten Format aus. Hier ist ein Beispiel für den Befehl und die Ausgabe:

```Bash
date +%d.%m.%Y
30.03.2021
```

In diesem Beispiel verwenden wir das Format `%d.%m.%Y`, um das Datum im Tag.Monat.Jahr-Format auszugeben. Es gibt viele verschiedene Formatierungsoptionen für das `date`-Kommando, die je nach Anforderungen verwendet werden können.

Eine weitere nützliche Option für den `date`-Befehl ist die `--date`-Option. Mit dieser Option können wir das Datum und die Uhrzeit für einen bestimmten Tag oder eine bestimmte Zeitspanne abrufen. Hier ist ein Beispiel:

```Bash
date --date="yesterday" +%A
Montag
```

Dieser Befehl gibt den Namen des Tages für gestern aus. Es gibt viele verschiedene Möglichkeiten, die `--date`-Option zu verwenden, um das gewünschte Datum oder die gewünschte Uhrzeit zu erhalten.

## Deep Dive

Der `date`-Befehl verwendet standardmäßig das aktuelle Datum und die aktuelle Uhrzeit des Systems. Dies kann jedoch durch die `--epoch`-Option geändert werden. Diese Option gibt das Datum in Epoch-Zeit aus, die die Anzahl der Sekunden seit dem 1. Januar 1970 um 00:00 Uhr UTC anzeigt.

Eine weitere Alternative, das aktuelle Datum abzurufen, ist die Verwendung von `cal`. Dieser Befehl zeigt den Kalender für das aktuelle Monat an und enthält auch das aktuelle Datum. Hier ist ein Beispiel:

```Bash
cal
	März 2021
Mo Di Mi Do Fr Sa So
 1  2  3  4  5  6  7
 8  9 10 11 12 13 14
15 16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31
```

## Siehe auch

- [Linuxize - So rufen Sie das aktuelle Datum in Bash ab](https://linuxize.com/post/bash-current-date/)
- [ShellHacks - Linux Datum und Zeit in der Bash](https://www.shellhacks.com/de/linux-get-date-time-current-date-time-date-command/)
- [The Linux Documentation Project - Date](https://tldp.org/LDP/abs/html/date.html)