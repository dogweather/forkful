---
title:    "Elixir: Vergleich von zwei Datumswerten"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum
Das Vergleichen von zwei Daten ist ein wichtiges Konzept in der Programmierung, insbesondere in Elixir. Es ermöglicht uns, Daten zu sortieren, filtern und validieren, was für eine saubere und effiziente Codebasis sorgt.

## Wie man es macht
Wir können zwei Daten auf verschiedene Arten in Elixir vergleichen. Zum Beispiel können wir die Operatoren ">", "<" und "==" verwenden, um zu überprüfen, ob eine Datei größer, kleiner oder gleich einer anderen ist. Schauen wir uns ein Beispiel an:

```elixir
date1 = ~D[2021-01-15]
date2 = ~D[2021-02-10]

date1 > date2 # false
date1 < date2 # true
date1 == date2 # false
```

In dem obigen Code haben wir zwei Daten erstellt und dann mithilfe der Operatoren verglichen. Elixir behandelt Datentypen wie Zahlen, was es uns ermöglicht, das Vergleichen auf diese Weise durchzuführen.

Es gibt auch die Möglichkeit, die Funktion `Date.compare/2` zu verwenden, um zwei Daten zu vergleichen. Diese Funktion gibt 0 zurück, wenn die Daten gleich sind, 1 wenn die erste Date größer ist und -1 wenn die zweite Date größer ist. Hier ist ein Beispiel:

```elixir
date1 = ~D[2021-01-15]
date2 = ~D[2021-02-10]

Date.compare(date1, date2) # -1
```

## Tiefergehende Informationen
Elixir behandelt Daten intern als Listen aus drei Elementen: Jahr, Monat und Tag. Wenn wir zwei Daten miteinander vergleichen, werden diese Listen verglichen. Aber was passiert, wenn zwei Daten den gleichen Tag haben? In diesem Fall wird die Reihenfolge der Vergleichsoperationen von Jahr zu Monat zu Tag durchgeführt.

Außerdem ist es wichtig zu beachten, dass die Funktion `Date.compare/2` auf internationale Standards für Datumsformatierungen achtet. Das bedeutet, dass das Vergleichen von zwei Daten, die in verschiedenen Ländern formatiert sind, dieselben Ergebnisse liefert.

## Siehe auch
- [Elixir Dokumentation zu Datum und Zeit](https://hexdocs.pm/elixir/Date.html)
- [Elixir Forum Diskussion über die Vergleichsfunktion von Daten](https://elixirforum.com/t/date-compare-ignores-timezone/29688)
- [Elixir School Tutorial zu Datum und Zeit](https://elixirschool.com/en/lessons/advanced/datetime/)