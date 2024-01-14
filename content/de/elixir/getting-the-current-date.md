---
title:                "Elixir: Das aktuelle Datum bekommen"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Haben Sie sich jemals gefragt, wie man das aktuelle Datum in Elixir erhält? Vielleicht müssen Sie es für eine Aufgabe oder eine Anwendung verwenden. Zum Glück ist es ein einfacher Prozess, der in nur wenigen Schritten erledigt werden kann.

# Wie geht man vor

Um das aktuelle Datum in Elixir zu erhalten, gibt es ein integriertes Modul namens `Calendar`. In diesem Modul steht uns die Funktion `now/0` zur Verfügung, die das Datum und die Zeit als Tupel zurückgibt.

Um diese Funktion zu verwenden, müssen wir jedoch zuerst das `Calendar`-Modul importieren:

```elixir
import Calendar
```

Anschließend können wir `now/0` aufrufen und das Ergebnis in einer Variablen speichern:

```elixir
current_date = now()
```

Jetzt haben wir das aktuelle Datum in der `current_date` Variable gespeichert. Um es auszugeben, können wir die Funktion `to_string/2` verwenden, die das Datum und die Uhrzeit im ISO-8601-Format zurückgibt:

```elixir
date_string = to_string(current_date, {:iso6801, :extended})
IO.puts(date_string)
```

Die Ausgabe könnte dann etwa so aussehen: `2021-05-05T15:30:00.000+02:00`

# Tiefergehende Informationen

Wenn Sie sich genauer mit dem `Calendar`-Modul beschäftigen, werden Sie feststellen, dass es viele nützliche Funktionen gibt, um mit Datum und Uhrzeit in Elixir umzugehen. Zum Beispiel können Sie mit der Funktion `now_to_date/1` das hierarchische Datum eines `DateTime` in ein `Date`-Objekt umwandeln oder mit der Funktion `add/3` eine bestimmte Anzahl an Tagen, Monaten oder Jahren zu einem Datum hinzufügen.

Eine detaillierte Beschreibung aller Funktionen finden Sie in der offiziellen Elixir-Dokumentation zum `Calendar`-Modul.

# Siehe auch

[Offizielle Elixir-Dokumentation zum Calendar-Modul](https://hexdocs.pm/elixir/Calendar.html)

[ElixirSchool-Lektion über Datum und Zeit](https://elixirschool.com/de/lessons/basics/dates-and-times/)

[Einführung in Elixir von Codecentric](https://www.codecentric.de/wissen/elixir-der-bessere-richtige-typ-gleiche-rechenleistung/)