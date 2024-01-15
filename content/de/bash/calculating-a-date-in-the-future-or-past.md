---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Bash: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum
Es gibt viele Gründe, warum man in der Bash Programmierung ein Datum in der Zukunft oder Vergangenheit berechnen würde. Vielleicht möchte man automatisierte Aufgaben ausführen, die an bestimmten Daten gebunden sind oder einfach nur das Datum für einen bestimmten Zweck in einem Skript berechnen.

# Wie geht das
Es gibt verschiedene Möglichkeiten, ein Datum in Bash zu berechnen. Eine Möglichkeit ist die Verwendung des Befehls `date`, der in den meisten Unix-basierten Systemen verfügbar ist. Hier ist ein Beispiel, wie man mithilfe von `date` ein Datum in der Zukunft berechnen kann:

```bash
date -d "+1 day"
```

Dieser Befehl gibt das Datum von morgen basierend auf dem aktuellen Datum aus. Man kann auch andere Zeitspannen angeben, zum Beispiel `+1 month` für das Datum in einem Monat.

Um ein Datum in der Vergangenheit zu berechnen, kann man einfach ein `-` vor der Zeitspanne verwenden, zum Beispiel `-1 day` für gestern.

# Tiefer eintauchen
Wenn man genauer in die Berechnung von Daten in der Bash eintauchen möchte, gibt es mehrere Faktoren zu beachten. Zum einen sollte man sich mit der Syntax des `date` Befehls vertraut machen und die verschiedenen Optionen ausprobieren. Zum anderen sollte man sich mit dem Konzept von Zeitstempeln, Zeitformaten und Zeitzonen vertraut machen, da diese eine wichtige Rolle bei der Berechnung von Daten spielen.

Es ist auch wichtig zu beachten, dass die Ergebnisse je nach System variieren können, da das verwendete Datumssystem und die Einstellungen der Zeitzonen variieren können. Deshalb ist es wichtig, die Ergebnisse zu überprüfen und sich mit den gewünschten Ergebnissen vertraut zu machen.

# Siehe auch
- [Bash Referenzhandbuch - date Befehl](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_10_02.html)
- [Unix Date Command Tutorial mit Beispielen](https://www.baeldung.com/linux/date-command)
- [Online-Datumsrechner für die Bash-Programmierung](https://alvinalexander.com/blog/post/linux-unix/how-calculate-dates-unix-linux-systems-shell-scripts/)