---
title:    "Fish Shell: Ein Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Vielleicht fragst du dich, warum du einem Blog-Post über "Fish Shell" und das Berechnen von zukünftigen oder vergangenen Daten lesen solltest. Nun, es gibt viele Anwendungsmöglichkeiten für diese Fähigkeit. Zum Beispiel kannst du damit eine Erinnerung für einen wichtigen Termin erstellen oder prüfen, ob ein bestimmtes Datum in der Vergangenheit ein Feiertag war.

## Wie man es macht

Um ein Datum in der Vergangenheit oder Zukunft zu berechnen, kannst du die im Fish Shell integrierten Funktionen "date" und "string" verwenden. Hier ist ein Beispiel für die Berechnung des Datums, das 5 Tage in der Zukunft liegt:

```Fish Shell
set future_date (date -f %Y-%m-%d -s "5 days")
echo $future_date
```

Dieser Code verwendet die Option "-f", um das Datum im Format "Jahr-Monat-Tag" auszugeben. Die Option "-s" legt die Anzahl der Tage fest, die dem aktuellen Datum hinzugefügt werden sollen. Du kannst diese Werte anpassen, um unterschiedliche zukünftige oder vergangene Daten zu berechnen.

## Tiefer ins Detail

Nun, da du weißt, wie du ein Datum in der Zukunft oder Vergangenheit berechnen kannst, wollen wir uns etwas genauer mit den Funktionen "date" und "string" befassen. "date" akzeptiert verschiedene Optionen, die es dir ermöglichen, das Datum in verschiedenen Formaten auszugeben. Zum Beispiel kannst du mit der Option "-u" auch die aktuelle Zeitzone ändern.

Die Funktion "string" dient dazu, Strings zu verarbeiten und zu konvertieren. In unserem Beispiel haben wir die Option "-s" verwendet, um die Anzahl der Tage festzulegen. Aber du kannst auch andere Zeiteinheiten wie Monate oder Jahre angeben. Weitere Informationen zu den verfügbaren Optionen findest du in der Fish Shell Dokumentation.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current)
- [Berechnen von Differenzen zwischen zwei Daten mit Fish Shell](https://dev.to/jhermanns/berechnen-von-differenzen-zwischen-zwei-daten-mit-fish-shell-3am7)