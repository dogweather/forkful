---
title:    "Elixir: Ein Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Datum kann für Elixir-Programmiererinnen und Programmierer sehr hilfreich sein, um beispielsweise Terminplanungen oder Ablaufsteuerungen zu implementieren.

## Wie man es macht

Die Elixir-Standardbibliothek bietet verschiedene Funktionen zur Berechnung von Datum und Zeit. Hier ist ein Beispiel, um ein Datum 5 Tage in der Zukunft zu berechnen:

```Elixir
{:ok, today} = Date.utc_today()
Date.add(today, 5)
```

Das Ergebnis davon ist ein Tupel der Form `{ :ok, future_date }`, wobei `future_date` das berechnete Datum darstellt. Ebenso kann ein vergangenes Datum berechnet werden, indem man die Anzahl der Tage mit einem Minuszeichen angibt.

Um Zeitintervalle zu berücksichtigen, gibt es auch die Funktion `DateTime.add/4`, die zusätzlich den Parameter `:minute` ermöglicht.

## Tiefere Einblicke

Elixir verwendet den Gregorianischen Kalender für die Berechnung von Datum und Zeit. Dieser Kalender basiert auf dem Sonnenjahr und berücksichtigt Schaltjahre.

Eine wichtige Sache, die bei der Berechnung von Zeitintervallen zu beachten ist, ist die Verwendung von Zeitzone in Elixir-Systemen. Die Funktionen `Date.utc_*` arbeiten mit der UTC-Zeitzone, während `Date.local_*` die lokale Zeitzone des Systems verwendet.

Um mehr über die Elixir-Datums- und Zeitfunktionen zu erfahren, empfehlen wir die offizielle Dokumentation.

## Siehe auch

- [Offizielle Elixir-Dokumentation zu Datums- und Zeitfunktionen](https://hexdocs.pm/elixir/Date.html)
- [Blog-Beitrag zum Elixir-Kalenderpaket](https://tech.euromoneyplc.com/elixir-calendar-package/)