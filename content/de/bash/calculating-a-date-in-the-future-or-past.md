---
title:                "Bash: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Es gibt zahlreiche Gründe, warum man ein Programm schreiben möchte, das ein Datum in der Zukunft oder Vergangenheit berechnet. Vielleicht planst du einen Urlaub und möchtest wissen, an welchem Datum du wieder zuhause sein wirst. Oder du möchtest eine Erinnerung setzen, um dich an wichtige Termine zu erinnern. Egal aus welchem Grund, die Berechnung von Datum kann eine nützliche Fähigkeit in der Bash-Programmierung sein.

## Wie geht das?

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, gibt es verschiedene Methoden in der Bash. Eine Möglichkeit ist die Nutzung des Befehls `date`, der standardmäßig auf vielen Unix- und Linux-Systemen verfügbar ist. Mit diesem Befehl kannst du das aktuelle Datum und Uhrzeit ausgeben lassen und auch in verschiedene Formate konvertieren. Zum Beispiel:

```Bash
date +%d.%m.%Y
```
Dieser Befehl gibt das heutige Datum im Format Tag.Monat.Jahr aus.

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir den Befehl `date` mit der Option `-d` verwenden. Hier können wir Parameter wie "next week", "2 years ago" oder spezifische Datumsangaben wie "25th of December" angeben. Zum Beispiel:

```Bash
date -d "next week"
```
Dieser Befehl gibt das Datum in einer Woche aus. Du kannst auch Datum und Uhrzeit kombinieren, um genauere Berechnungen zu erhalten, z.B.:

```Bash
date -d "2 weeks 3 days 10:00" +%d.%m.%Y
```
Dieser Befehl gibt das Datum in 2 Wochen, 3 Tagen und um 10 Uhr aus, im Format Tag.Monat.Jahr.

## Tiefere Einblicke

Der Befehl `date` kann auch mit der Option `-s` verwendet werden, um das Systemdatum und die Uhrzeit manuell zu setzen. Dies kann nützlich sein, um bestimmte Datumskonfigurationen zu testen, ohne das eigentliche Systemdatum zu ändern. Zum Beispiel:

```Bash
date -s "2030-01-01 12:00:00"
```
Dieser Befehl setzt das Datum auf den 1. Januar 2030 um 12 Uhr mittags.

Eine weitere nützliche Funktion ist die Möglichkeit, Datumsbereiche zu vergleichen. Mit den Vergleichsoperatoren wie `<` oder `>` können wir überprüfen, ob ein Datum vor oder nach einem anderen Datum liegt. Zum Beispiel:

```Bash
if [ "2020-05-01" > "2020-04-30" ]; then echo "Das zweite Datum liegt nach dem ersten."; fi
```
Dieses Beispiel würde die Nachricht ausgeben, dass das zweite Datum nach dem ersten liegt.

Mit diesen und vielen weiteren Funktionen des `date`-Befehls kannst du deine Bash-Skripte um die Fähigkeit erweitern, Datum in der Zukunft oder Vergangenheit zu berechnen und damit deine täglichen Aufgaben effizienter zu gestalten.

## Siehe auch
- [Bash-Befehlsreferenz: date](https://www.gnu.org/software/coreutils/date/) 
- [Shell Scripting Tutorial - Date Manipulation](https://www.shellscript.sh/date.html)