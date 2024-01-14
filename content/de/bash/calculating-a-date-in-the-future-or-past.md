---
title:                "Bash: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann in vielen Szenarien nützlich sein, z.B. bei der Planung von Terminen oder Veranstaltungen oder bei der Erstellung von automatisierten Skripten. Es ist daher wichtig zu wissen, wie man dies in der Bash-Programmierung durchführen kann.

## Wie geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, verwenden wir das `date` Kommando in Kombination mit der Option `-d` und dem gewünschten Datumsformat. Ein Beispielcode könnte so aussehen:

```Bash
date -d "27 September 2021" +"Der Tag in einer Woche ist %A, der 4. Oktober 2021"
```

Dies würde uns das Ergebnis "Der Tag in einer Woche ist Montag, der 4. Oktober 2021" anzeigen. Beachten Sie, dass wir das gewünschte Datum im Zitatzeichen angeben und das gewünschte Datumsformat mit `+` angeben.

## Deep Dive

Die Verwendung von Optionen wie `-d` ermöglicht es uns, komplexe Berechnungen durchzuführen, um z.B. das Datum von einem bestimmten Tag aus zu bestimmen oder um die Zeitzone zu berücksichtigen. Wenn Sie tiefer in die Materie eintauchen möchten, können Sie die Dokumentation des `date` Kommandos lesen und verschiedene Datumsformate ausprobieren.

## Siehe auch

- [Bash-Programmierung lernen: Ein umfassender Leitfaden](https://www.linode.com/docs/guides/beginning-bash-scripting-guide/)
- [Die `date`-Dokumentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Ein nützliches Tool zum Berechnen von Datumsangaben in der Bash-Programmierung](https://ostechnix.com/dealynum-bash-script-display-date-and-time-differences/)