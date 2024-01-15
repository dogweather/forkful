---
title:                "Den aktuellen Datum erhalten"
html_title:           "Bash: Den aktuellen Datum erhalten"
simple_title:         "Den aktuellen Datum erhalten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Wenn du wissen möchtest welches Datum heute ist, musst du nicht mehr den Kalender aufhängen oder auf deiner Uhr nachsehen. Mit Bash, der aktuellen Version des Bourne Again SHell, kannst du ganz einfach das aktuelle Datum und die aktuelle Zeit auf deinem Computer anzeigen lassen.

## Wie geht's

Um das aktuelle Datum und die Zeit in Bash zu bekommen, gibt es mehrere Möglichkeiten. Hier sind zwei Beispiele mit jeweils einer unterschiedlichen Ausgabe:

```Bash
# Aktuelles Datum im Format "Tag. Monat Jahr"
date +'%d. %m %Y' 
```
Die Ausgabe wäre zum Beispiel "05. 10 2021".

```Bash
# Aktuelles Datum und Zeit im Format Jahr-Monat-Tag-Stunde-Minute-Sekunde
date +'%Y-%m-%d-%H-%M-%S'
```
Die Ausgabe wäre zum Beispiel "2021-10-05-12-30-45".

Bei beiden Beispielen kannst du das Format anpassen, indem du die Zeichenfolge zwischen den einfachen Anführungszeichen änderst. Die Bedeutung der einzelnen Zeichen ist in der Bash-Dokumentation genau beschrieben.

## Tiefer Einblick

Die Befehle, die wir oben benutzt haben, basieren auf dem Datum und der Zeit, die im Betriebssystem deines Computers gespeichert sind. Sie werden normalerweise als "Unix-Time" bezeichnet und zählen die Sekunden seit dem 1. Januar 1970. Wenn du den Unix Time Stamp kennen möchtest, kannst du ihn direkt in Bash abrufen:

```Bash
# Aktueller Unix-Time Stamp
date +%s
```
Die Ausgabe wäre zum Beispiel "1633461200".

Außerdem kannst du mit Bash auch das Datum in der Zukunft oder Vergangenheit berechnen. Zum Beispiel kannst du das Datum von vor 10 Tagen mit folgendem Befehl bekommen:

```Bash
# Aktuelles Datum minus 10 Tage
date -d "10 day ago" +'%d. %m %Y'
```
Die Ausgabe wäre zum Beispiel "25. 09 2021". Du kannst auch Wochentage und Monate hinzufügen oder subtrahieren und damit verschiedene Datumsberechnungen durchführen.

## Siehe auch

- [Bash-Dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Unix-Time in Bash](https://www.cyberciti.biz/faq/linux-unix-get-current-unixtime-from-command-prompt-cli/)
- [Datumsberechnungen mit Bash](https://www.tutorialspoint.com/unix_commands/date.htm)