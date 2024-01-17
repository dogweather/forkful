---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Elixir: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Berechnung eines Datums in der Zukunft oder Vergangenheit ist eine häufige Aufgabe für Programmierer. Dabei handelt es sich um eine Funktion, die ein bestimmtes Datum um eine festgelegte Anzahl von Tagen, Wochen, Monaten oder Jahren verschiebt. Programme benötigen diese Funktion, um zukünftige Termine zu planen oder vergangene Ereignisse zu verfolgen.

## Wie geht's?
Um ein Datum in Elixir zu berechnen, kannst du die Funktion `Date.add/2` verwenden. Sie nimmt zwei Argumente an: das ursprüngliche Datum und die Anzahl der Tage, Wochen, Monate oder Jahre, um die es verschoben werden soll. Zum Beispiel:`ElixirDate.add({2021, 6, 1}, 2)` ergibt das Datum 3. Juni 2021.

Um eine bestimmte Einheit zu verwenden, kannst du zusätzliche Funktionen verwenden, wie z.B. `Date.add_days/2`, `Date.add_weeks/2`, `Date.add_months/2` und `Date.add_years/2`. Zum Beispiel:`ElixirDate.add_weeks({2021, 6, 1}, 2)` ergibt das Datum 15. Juni 2021.

## Tiefer Einblick
Die Berechnung von Daten in der Zukunft oder Vergangenheit ist ein häufiges Problem in der Programmierung und es gibt viele verschiedene Ansätze, um es zu lösen. Manche Programmiersprachen haben integrierte Funktionen, um dies zu tun, während andere auf externe Bibliotheken angewiesen sind. In Elixir ist es einfach dank der eingebauten `Date`-Klasse.

Es ist auch wichtig zu beachten, dass bei der Berechnung von Daten in der Zukunft oder Vergangenheit Faktoren wie Schaltjahre und Schaltmonate berücksichtigt werden müssen. In Elixir können diese komplexen Berechnungen mit der `Date.shift`-Funktion durchgeführt werden.

## Sieh dir auch an
- [Elixir-Dokumentation zu Datum und Zeit](https://hexdocs.pm/elixir/Date.html)
- [Einführung in Elixir: Eine funktionale Sprache für elegante Codierung](https://www.educba.com/introduction-to-elixir/)