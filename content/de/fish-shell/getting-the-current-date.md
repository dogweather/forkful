---
title:                "Das aktuelle Datum erhalten"
html_title:           "Fish Shell: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Manchmal braucht man das aktuelle Datum und die Uhrzeit für bestimmte Aktionen, wie zum Beispiel für die Verwendung als Dateinamen oder für das Protokollieren von Ereignissen. Mit Fish Shell ist es einfach, das aktuelle Datum und die Uhrzeit in verschiedenen Formaten zu bekommen.

## How To

Um das aktuelle Datum und die Uhrzeit in Fish Shell zu erhalten, verwende den Befehl `date`, gefolgt von der gewünschten Formatierung. Hier sind einige Beispiele:

```
Fish Shell: date +%m/%d/%Y
Output: 09/16/2021

Fish Shell: date "+%A, %B %d, %Y"
Output: Donnerstag, September 16, 2021

Fish Shell: date +%H:%M:%S
Output: 18:33:14
```

Du kannst auch mehrere Formatierungen kombinieren, indem du sie in Anführungszeichen trennst, wie im zweiten Beispiel gezeigt. Die volle Liste der verfügbaren Formatierungen findest du in der Fish Shell-Dokumentation oder indem du den Befehl `date --help` ausführst.

## Deep Dive

Die `date`-Funktion in Fish Shell verwendet die GNU-Version des `date`-Befehls, die oft als `gdate` bezeichnet wird. Du kannst jedoch auch die POSIX-Version verwenden, indem du `gdate` im Befehl durch `date` ersetzt. Ein weiteres nützliches Flag ist `-d`, mit dem du ein bestimmtes Datum angeben kannst, von dem aus die aktuelle Zeit berechnet wird. Zum Beispiel:

```
Fish Shell: gdate -d "2020/06/12 10:00:00" +%H:%M:%S
Output: 10:00:00
```

Dieser Befehl würde die aktuelle Uhrzeit basierend auf dem angegebenen Datum und der Uhrzeit berechnen. Du kannst auch relative Angaben wie "1 hour ago" oder "next Friday" verwenden.

## Siehe auch

- [Fish Shell-Dokumentation](https://fishshell.com/docs/current/cmds/date.html)
- [Liste der verfügbaren `date`-Formatierungen](http://man7.org/linux/man-pages/man1/date.1.html#Format%20interpolation)
- [GNU-`date`-Dokumentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)