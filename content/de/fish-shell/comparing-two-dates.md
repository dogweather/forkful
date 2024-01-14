---
title:                "Fish Shell: Vergleich von zwei Datum"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist oft eine hilfreiche Aufgabe in der Programmierung. Es kann dabei helfen, bestimmte Aktionen oder Funktionen durchzuführen, abhängig von der Beziehung zwischen den Daten. In dieser Anleitung werden wir zeigen, wie man dies mithilfe der Fish Shell erreichen kann.

## Wie Geht's?

Um zwei Daten in der Fish Shell zu vergleichen, können wir den Befehl `test` oder `[` verwenden. Wir geben die beiden Daten als Argumente an und vergleichen sie mit Hilfe von Operatoren wie `>` (größer als), `<` (kleiner als) oder `=` (gleich). Je nach Ergebnis gibt der Befehl eine `0` für "wahr" oder `1` für "falsch" zurück.

```Fish Shell
set today (date +%s)  # heutiges Datum in Sekunden
set tomorrow (date -d "+1 day" +%s)  # Datum von morgen in Sekunden

if test $today -gt $tomorrow  # Vergleich: ist heute größer als morgen?
    echo "Heute ist später als morgen!"
end

### Ausgabe: Heute ist später als morgen!
```

In diesem Beispiel haben wir mit `test` das heutige Datum mit dem Datum von morgen verglichen und da es später ist, wurde die Ausgabe entsprechend angepasst.

## Tiefer Eintauchen

Mit der Fish Shell können wir auch komplexere Vergleiche durchführen, indem wir Verkettungen von mehreren `test` Befehlen verwenden. Zum Beispiel könnten wir feststellen, ob eine bestimmte Uhrzeit in einem Zeitbereich liegt oder ob ein Datum in einem bestimmten Monat liegt.

Weitere Informationen und Beispiele zu `test` und anderen Befehlen in der Fish Shell können in der offiziellen Dokumentation gefunden werden.

## Siehe Auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [Bash: Vergleich von Datumsangaben](https://www.gnu.org/software/bash/manual/html_node/Conditional-Constructs.html#Conditional-Constructs)
- [Vergleich von Daten in der Python Programmierung](https://www.python.org/dev/peps/pep-0507/)