---
title:                "Vergleich von zwei Datumsangaben"
html_title:           "Fish Shell: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal vor der Herausforderung standest, zwei verschiedene Daten miteinander zu vergleichen, bist du hier genau richtig. Dieser Artikel zeigt dir, wie du mit dem Fish Shell einfach und effizient Datumsangaben vergleichen kannst.

## How To

```Fish Shell
# Standard-Dateiformat
set start_date '2021-01-01'
set end_date '2021-12-31'

# Vergleiche die beiden Daten
if [ $start_date > $end_date ]
  echo "Das Startdatum liegt nach dem Enddatum."
else
  echo "Das Startdatum liegt vor dem Enddatum." 
end
```

Der obige Code illustriert die grundlegende Syntax zum Vergleichen von zwei Daten im Fish Shell. Zuerst werden die beiden Daten in Variablen gespeichert und dann mithilfe einer einfachen if-Kondition überprüft. Je nach Ergebnis wird eine entsprechende Meldung ausgegeben.

```Fish Shell
# Erweiterter Vergleich mit Uhrzeitangaben
set start_date '2021-06-01T12:00:00'
set end_date '2021-06-01T18:00:00'

function compare_dates
  if [ (date -d "$1" +%s) -lt (date -d "$2" +%s) ]
    echo "Das Startdatum liegt vor dem Enddatum."
  else
    echo "Das Startdatum liegt nach dem Enddatum."
  end
end

compare_dates $start_date $end_date
```

In diesem Beispiel wird die Funktion `compare_dates` verwendet, um zwei Daten mit Uhrzeitangaben zu vergleichen. Mithilfe des `date`-Befehls werden die Daten in Sekunden umgewandelt und dann verglichen. Dies ermöglicht einen genaueren Vergleich, der auch mit Uhrzeitangaben funktioniert.

## Deep Dive

Das Vergleichen von Daten mag vielleicht einfach erscheinen, doch es gibt einige Fallstricke, die man dabei beachten sollte. Einer davon ist das Dateiformat der Daten. Das Standard-Dateiformat im Fish Shell ist `yyyy-mm-dd`, daher ist es wichtig, dass die Daten in diesem Format vorliegen, bevor sie verglichen werden. Ansonsten kann es zu unerwarteten Ergebnissen führen.

Ein weiteres wichtiges Detail ist die Zeitzone. Wenn du mit verschiedenen Zeitzone arbeitest, solltest du darauf achten, dass alle Daten in derselben Zeitzone angegeben werden, um korrekte Vergleiche durchzuführen. Ansonsten können Zeitunterschiede zu falschen Ergebnissen führen.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial für Einsteiger](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell Best Practices](https://fishshell.com/docs/current/best_practices.html)