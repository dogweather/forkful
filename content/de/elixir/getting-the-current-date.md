---
title:    "Elixir: Das aktuelle Datum erhalten"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

Mein Name ist Anna und ich werde heute über die Programmiersprache Elixir schreiben. Elixir ist eine funktionale Programmiersprache, die für ihre Skalierbarkeit und Ausfallsicherheit bekannt ist. In diesem Blogpost gebe ich euch eine Einführung in das Arbeiten mit Datum in Elixir.

## Warum

Das Abrufen des aktuellen Datums mag auf den ersten Blick keine wichtige Aufgabe erscheinen, aber es kann sehr nützlich sein, wenn wir Anwendungen entwickeln, die beispielsweise Zeiträume oder Verfallsdaten verarbeiten müssen. Auch für die Fehlerbehebung kann das aktuelle Datum sehr hilfreich sein.

## Wie man das aktuelle Datum in Elixir erhält

Das Abrufen des aktuellen Datums in Elixir ist sehr einfach. Dafür verwenden wir die Funktion `DateTime.utc_now()`, die uns das aktuelle Datum im UTC-Format zurückgibt. Wir können dann die Funktion `DateTime.to_local()` verwenden, um das Datum in unsere lokale Zeitzone zu konvertieren.

```Elixir
DateTime.utc_now()  # => ~U[2021-05-04 16:30:00Z]
DateTime.to_local(DateTime.utc_now())  # => ~U[2021–05-04 18:30:00+02:00]
```

Wir können auch nur das Datum ohne Uhrzeit erhalten, indem wir die Funktion `Date.utc_today()` verwenden.

```Elixir
Date.utc_today()  # => ~D[2021-05-04]
```

## Tiefere Einblicke

Wenn wir in Elixir mit Datum arbeiten, verwenden wir normalerweise die `DateTime`- und `Date`-Module aus der Standardbibliothek. Diese Module bieten viele Funktionen, um mit Datum und Uhrzeit zu arbeiten. Einige nützliche Funktionen sind zum Beispiel `DateTime.add`, um eine bestimmte Anzahl von Tagen, Stunden oder Minuten zu einem Datum hinzuzufügen, oder `DateTime.diff`, um die Differenz zwischen zwei Datumswerten zu berechnen.

Es gibt auch ein praktisches Tool namens Ecto, das häufig in Elixir-Projekten verwendet wird und eine Datenbank-Abstraktionsschicht bietet. Ecto bietet auch Unterstützung für das Arbeiten mit Datum und Zeit und verschiedene Funktionen, um Datums- und Zeitspalten in Datenbankabfragen zu verwenden.

## Siehe auch

- [Elixir-Dokumentation zum Datum](https://hexdocs.pm/elixir/DateTime.html)
- [Ecto-Dokumentation zum Datum und Zeit](https://hexdocs.pm/ecto/Ecto.DateTime.html)
- [Elixir School: Datums- und Zeitmanipulation](https://elixirschool.com/de/lessons/basics/date-time/)