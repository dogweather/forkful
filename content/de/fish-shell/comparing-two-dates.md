---
title:                "Fish Shell: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man das Vergleichen von zwei Daten in der Fish-Shell lernen sollte. Zum Beispiel hilft es dabei, Termine und Aufgaben zu organisieren und zu priorisieren. Auch bei der Datenanalyse kann es hilfreich sein, verschiedene Zeiträume miteinander zu vergleichen. In diesem Blog-Beitrag werden wir uns genau damit beschäftigen und zeigen, wie einfach es ist, mit der Fish-Shell zwei Daten zu vergleichen.

## Wie geht's

Um zwei Daten in der Fish-Shell zu vergleichen, gibt es verschiedene Möglichkeiten. Die einfachste Methode ist die Verwendung des Befehls "date". Um zum Beispiel das heutige Datum zu erhalten, nutzen wir folgenden Code:

```Fish Shell
date +%Y-%m-%d
```

Das Ergebnis wird in diesem Format angezeigt: 2020-05-20. Um zwei Daten zu vergleichen, können wir nun einfach die Dateien mit ">=" oder "<=" verknüpfen. Beispielsweise möchten wir überprüfen, ob das Datum von gestern größer oder gleich dem heutigen Datum ist. Dazu nutzen wir folgenden Code:

```Fish Shell
date --date="yesterday" +%Y-%m-%d >= date +%Y-%m-%d
```

Das Ergebnis wird entweder "1" (wahr) oder "0" (falsch) sein, je nachdem, ob das Datum von gestern größer oder gleich dem heutigen Datum ist.

## Tiefere Einblicke

In der Fish-Shell gibt es noch viele andere Möglichkeiten, um zwei Daten zu vergleichen. Zum Beispiel können wir Zeiträume in Sekunden umwandeln und vergleichen, oder wir können mit dem Befehl "strftime" bestimmte Datumsformate erzeugen und diese miteinander vergleichen. Eine ausführliche Anleitung dazu findest du in der offiziellen Dokumentation der Fish-Shell unter [https://github.com/fish-shell/fish-shell/blob/master/share/doc/fish/commands/date.md](https://github.com/fish-shell/fish-shell/blob/master/share/doc/fish/commands/date.md).

## Siehe auch

- Offizielle Dokumentation der Fish-Shell: https://fishshell.com/docs/current/index.html
- Vergleiche von Shell-Skripts: https://blog.fishshell.com/2020/04/08/shell-script-comparison/
- Verwendung von Zeitspannen in der Fish-Shell: https://www.linuxtechi.com/measure-time-difference-bash-shell-scripting/