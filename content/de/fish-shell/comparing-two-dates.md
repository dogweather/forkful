---
title:    "Fish Shell: Vergleich von zwei Datumsangaben"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist eine gängige Aufgabe in der Programmierung. Es kann hilfreich sein, um festzustellen, ob ein bestimmtes Datum vor oder nach einem anderen liegt, oder um zu überprüfen, ob ein Ereignis in einem bestimmten Zeitraum stattgefunden hat. In diesem Blog-Beitrag werden wir uns ansehen, wie man dies mit dem Fish Shell durchführen kann.

## Anleitung

Um zwei Daten miteinander zu vergleichen, benötigen wir zunächst zwei Variablen, die jeweils ein Datum enthalten. Wir können diese Variablen mit dem Befehl "set" erstellen und ihnen ein Datum im Format "YYYY-MM-DD" zuweisen.

```
Fish Shell
set today 2021-07-15
set deadline 2021-07-23
```

Um zu überprüfen, ob heute vor dem angegebenen Termin liegt, können wir den Befehl "test" verwenden und die Optionen "-lt" (für "less than") und "-eq" (für "equal") angeben.

```
Fish Shell
test $today -lt $deadline; and echo "Heute liegt vor dem Termin."
```

In diesem Beispiel werden wir die Ausgabe "Heute liegt vor dem Termin." sehen, da das heutige Datum vor dem angegebenen Termin liegt.

## Tiefergehende Informationen

Wenn wir einen genaueren Vergleich zwischen zwei Daten durchführen möchten, können wir die integrierten Funktionen von Fish Shell verwenden. Eine nützliche Funktion ist "date -d", die es uns ermöglicht, ein Datum im Unix-Zeitformat zu erhalten. Dies kann hilfreich sein, wenn wir Datumsangaben in Sekunden oder Millisekunden benötigen.

Eine weitere nützliche Funktion ist "math" für mathematische Berechnungen. Mit dieser Funktion können wir beispielsweise die Anzahl der Tage zwischen zwei Daten berechnen.

```
Fish Shell
set today (date -d $today +%s)
set deadline (date -d $deadline +%s)
set difference (math $deadline - $today)
echo "Es sind $difference Sekunden zwischen heute und dem Termin."
```

Die Ausgabe dieses Beispiels wird die Anzahl der Sekunden zwischen den beiden Daten ausgeben.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Github Repository](https://github.com/fish-shell/fish-shell)
- [Fish Shell Cheatsheet](https://gist.github.com/rogual/f4c19455ba78dd422cd4)