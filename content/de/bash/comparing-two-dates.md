---
title:                "Bash: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Die Vergleichung von zwei Daten ist eine nützliche Fähigkeit in der Bash Programmierung, die es ermöglicht, bestimmte Aktionen basierend auf den zeitlichen Informationen auszuführen. Zum Beispiel kann man durch Vergleichen von Daten in einem Skript überprüfen, ob eine Datei geändert wurde oder um zu überprüfen, ob ein bestimmter Zeitraum verstrichen ist.

# Wie geht es

Die Vergleichung von zwei Daten in Bash ist relativ einfach und erfordert die Verwendung von Befehlen und Operatoren. Im Folgenden finden Sie einige Beispiele, wie Sie zwei Daten miteinander vergleichen können.

### Datum im Dateinamen überprüfen

```Bash
# Überprüfen, ob die Datei in den letzten 24 Stunden geändert wurde
if [[ $(find test.txt -mtime -1) ]]; then
  echo "Die Datei wurde in den letzten 24 Stunden geändert"
fi
```

In diesem Beispiel verwenden wir den Befehl `find`, um nach einer bestimmten Datei zu suchen, und mit der Option `-mtime` können wir angeben, wie viele Tage seit der letzten Änderung vergangen sind. Wenn die Bedingung erfüllt ist, wird der Text "Die Datei wurde in den letzten 24 Stunden geändert" ausgegeben.

### Datumsvergleich mit logischen Operatoren

```Bash
# Überprüfen, ob das aktuelle Datum innerhalb eines bestimmten Zeitraums liegt
if [[ $(date +%Y%m%d) -gt 20211001 && $(date +%Y%m%d) -lt 20211031 ]]; then
  echo "Das aktuelle Datum liegt im Oktober 2021"
fi
```

Hier verwenden wir den Befehl `date` zusammen mit dem Operator `%Y%m%d`, um das aktuelle Datum im Format "JahrMonatTag" zu erhalten. Dann vergleichen wir es mit den gewünschten Daten eingebettet in logische Operatoren wie `&&` und `||`.

# Tiefgehende Details

Wenn es um das Vergleichen von Daten geht, gibt es einige wichtige Punkte zu beachten. Zum Beispiel müssen die Daten im gleichen Format vorliegen, um einen genauen Vergleich durchzuführen. Außerdem können komplexe Vergleiche durch Kombination mehrerer Befehle und Operatoren erreicht werden.

Ein weiterer wichtiger Aspekt ist, dass das Datum in Unix-Zeitstempel umgewandelt werden kann, um das Vergleichen zu erleichtern. Hier ist ein Beispiel, wie Sie das aktuelle Datum in Unix-Zeitstempel umwandeln und mit einem anderen Unix-Zeitstempel vergleichen können:

```Bash
# Konvertierung des aktuellen Datums in Unix-Zeitstempel
current_date=$(date +%s)

# Vergleich mit einem bestimmten Unix-Zeitstempel (z.B. 01.12.2021 um 12 Uhr)
if [[ $current_date -gt 1638367200 ]]; then
  echo "Das aktuelle Datum liegt nach dem 1. Dezember 2021"
fi
```

In diesem Beispiel wird das aktuelle Datum mit einem festgelegten Unix-Zeitstempel verglichen. Der Vorteil der Verwendung von Unix-Zeitstempeln liegt darin, dass sie unabhängig vom Datumsformat immer gleich sind.

# Siehe auch

- Datums- und Zeitfunktionen in Bash: https://www.tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-7.html
- Unix-Zeitstempel: https://de.wikipedia.org/wiki/Unix-Zeit
- Shell-Befehl `date`: https://www.tutorialspoint.com/unix_commands/date.htm