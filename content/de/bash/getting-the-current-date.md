---
title:                "Bash: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist ein gemeinsames Szenario in der Bash-Programmierung. Es ermöglicht das Organisieren von Dateien basierend auf dem Datum und die Formatierung von Ausgaben. In diesem Blog-Beitrag erfahren Sie, wie Sie das aktuelle Datum in Ihrer Bash-Skripting-Umgebung erhalten.

## Wie geht das?

Um das aktuelle Datum in Bash zu erhalten, können wir das Befehlszeilenprogramm "date" verwenden. Wir können verschiedene Optionen verwenden, um das Datum in verschiedenen Formaten zu erhalten. Schauen wir uns einige Beispiele an.

```Bash
# Datum im Format "Monat Tag, Jahr"
date +"%B %d, %Y" 
# Ausgabe: Mai 25, 2021

# Datum im Format "Tag/Monat/Jahr"
date +"%d/%m/%Y" 
# Ausgabe: 25/05/2021

# Datum im Format "Wochentag, Monat Tag, Jahr"
date +"%A, %B %d, %Y" 
# Ausgabe: Dienstag, Mai 25, 2021
```

Wir können auch die Option "now" verwenden, um das aktuelle Datum und die aktuelle Uhrzeit zu erhalten.

```Bash
# Aktuelles Datum und Uhrzeit
date +"%c" 
# Ausgabe: Di 25 Mai 2021 10:30:45 CEST
```

Weitere Optionen und Formate finden Sie in der Dokumentation zum "date" Befehl.

## Tiefer Einblick

Das "date" Befehlszeilenprogramm nutzt die Systemzeit, um das aktuelle Datum und die aktuelle Uhrzeit zu erhalten. Die Systemzeit wird in POSIX-Zeitstempel (Sekunden seit dem 01. Januar 1970) gemessen. Das "date" Programm wandelt diesen Zeitstempel in ein für uns lesbares Format um.

Wir können auch die Systemzeit direkt mit dem Befehl "date +%s" abrufen.

```Bash
# Aktuelle Systemzeit in Sekunden seit 01.01.1970
date +"%s" 
# Ausgabe: 1621929027
```

Es ist auch möglich, die Systemzeit zu ändern, indem man die Hardware-Uhr (RTC) anpasst. Dies sollte jedoch mit Vorsicht durchgeführt werden, da dies Auswirkungen auf andere Prozesse und Systemfunktionen haben kann.

## Siehe auch

- [GNU Coreutils - Teil 2: Datum und Zeit](https://wiki.ubuntuusers.de/GNU_Coreutils_-_Teil_2:_Datum_und_Zeit/)
- [Bash-Dokumentation - Datum und Uhrzeit](https://www.gnu.org/software/bash/manual/html_node/Date-and-Time.html)
- [Zeitmanipulation unter Linux mit "date"](https://www.linux-magazin.de/ausgaben/2007/06/zeitmanipulation/)