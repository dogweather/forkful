---
title:    "Bash: Eine Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Warum

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann nützlich sein, um Termine oder Fristen im Voraus zu planen oder die Vergangenheit zu überprüfen.

##Wie geht's?

Um ein Datum in der Zukunft zu berechnen, können wir den Befehl `date` in Bash verwenden und das gewünschte Datum mithilfe des Parameters `-d` angeben. Zum Beispiel:

```Bash
date -d "1 week"
```

Dies gibt das Datum von heute in einer Woche aus. Wir können auch Ausdrücke wie "next Friday" oder "tomorrow" verwenden, um ein spezifisches Datum zu erhalten.

Um ein Datum in der Vergangenheit zu berechnen, verwenden wir den gleichen Befehl, aber mit dem Parameter `-d` gefolgt von "ago". Zum Beispiel:

```Bash
date -d "1 month ago"
```

Dies gibt das Datum von vor einem Monat aus. Wir können auch verschiedene Einheiten wie Jahre, Monate oder Tage verwenden, um ein spezifisches Datum in der Vergangenheit zu erhalten.

##Tief eintauchen

Der `date` Befehl bietet viele verschiedene Möglichkeiten, um ein spezifisches Datum in der Zukunft oder Vergangenheit zu berechnen. Zum Beispiel können wir eine bestimmte Uhrzeit angeben, indem wir `HH:MM:SS` an das Datum anhängen:

```Bash
date -d "next Friday 13:00:00"
```

Wir können auch das Datum anhand der aktuellen Zeit berechnen, indem wir das heutige Datum verwenden und eine gewisse Anzahl von Stunden, Minuten oder Sekunden hinzufügen oder subtrahieren.

Es ist auch möglich, die Sprache für die Ausgabe des Datums anzugeben, indem wir den Parameter `-l` gefolgt von der entsprechenden Sprachabkürzung angeben. Zum Beispiel:

```Bash
date -d "next month" -l de_DE
```

Dies gibt das Datum in deutscher Sprache aus.

##Siehe auch

- [Linuxize - How to Use the `date` Command in Linux](https://linuxize.com/post/how-to-use-date-command-in-linux/)
- [Computer Hope - Linux `date` command](https://www.computerhope.com/unix/udate.htm)
- [IBM - Using the `date` command](https://www.ibm.com/docs/en/aix/6.1?topic-s=date-command)