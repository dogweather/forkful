---
title:                "Fish Shell: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Im Alltag kann es immer wieder vorkommen, dass man ein bestimmtes Datum in der Zukunft oder Vergangenheit berechnen muss. Zum Beispiel, um einen Geburtstag zu planen oder eine Reise zu organisieren. Mit der Fish Shell kann dies schnell und einfach erledigt werden.

## How To

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, muss zuerst das aktuelle Datum ermittelt werden. Dies kann mit dem Befehl `date` erledigt werden. Der Befehl `date +%s` gibt den aktuellen Zeitstempel in Sekunden seit dem 1. Januar 1970 zurück.

```Fish Shell
set current_date (date +%s)
```

Um das gewünschte Datum zu berechnen, können nun verschiedene mathematische Operationen durchgeführt werden. Zum Beispiel, um das Datum für den nächsten Monat zu berechnen, kann 2.592.000 Sekunden (30 Tage) zum aktuellen Zeitstempel addiert werden.

```Fish Shell
set future_date (math $current_date + 2592000)
```

Das Ergebnis wird wieder als Zeitstempel ausgegeben. Um es in ein lesbares Datum umzuwandeln, kann der Befehl `date -d @<time stamp>` verwendet werden. Der Befehl `-d` gibt das Datum im ISO-Format aus. Um es in einem anderen Format auszugeben, kann die Option `+<format>` verwendet werden. Zum Beispiel `date -d @<time stamp> +%D` für das US-amerikanische Datumsformat (MM/DD/YY). Das Endergebnis könnte dann so aussehen:

```Fish Shell
date -d @$future_date +%D

04/05/21
```

## Deep Dive

Um ein Datum in der Zukunft oder Vergangenheit genauer zu berechnen, kann man sich mit den verschiedenen Datumsangaben und Rechenoperationen befassen. Zum Beispiel können neben Sekunden auch Minuten (`m`), Stunden (`h`) oder Tage (`d`) verwendet werden. Außerdem gibt es die Möglichkeit, eine bestimmte Anzahl von Jahren, Monaten oder Tagen zum aktuellen Datum hinzuzufügen oder davon abzuziehen.

Weitere Informationen und Beispiele zur Verwendung der Befehle `date` und `math` finden Sie in der offiziellen Dokumentation der Fish Shell.

## Siehe auch

- [Offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [BashBlog - So schreiben Sie einen Blog-Parser für die Fish Shell](https://dev.to/clivern/how-to-write-blog-parser-for-fish-shell-using-bashblog-5f0m)
- [Unix Date Befehl verwenden](https://www.linuxjourney.com/lesson/date)