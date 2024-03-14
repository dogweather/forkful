---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:23.043432-07:00
description: "Das Abrufen des aktuellen Datums in Elixir umfasst den Zugriff auf die\
  \ Datums- und Zeitinformationen des Systems, eine allgemeine Aufgabe f\xFCr das\
  \ Logging,\u2026"
lastmod: '2024-03-13T22:44:53.545535-06:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in Elixir umfasst den Zugriff auf die Datums-\
  \ und Zeitinformationen des Systems, eine allgemeine Aufgabe f\xFCr das Logging,\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in Elixir umfasst den Zugriff auf die Datums- und Zeitinformationen des Systems, eine allgemeine Aufgabe für das Logging, Datumsstempel oder jede Funktionalität, die Kenntnisse über das aktuelle Datum erfordert. Diese Operation ist wesentlich für die Erstellung zeitbewusster Anwendungen und für Aufgaben wie das Generieren von Berichten oder Zeitstempeln in einer Webanwendung.

## Wie:
Die Standardbibliothek von Elixir ermöglicht über das `DateTime`-Modul das Abrufen des aktuellen Datums und der aktuellen Uhrzeit. Da Elixir auf der Erlang VM (BEAM) läuft, nutzt es die zugrundeliegenden Erlang-Funktionalitäten für Zeitoperationen.

### Verwendung der Standardbibliothek von Elixir
Elixir stellt die Funktion `DateTime.utc_now/0` zur Verfügung, um das aktuelle Datum und Uhrzeit in UTC zu erhalten.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Beispielausgabe:**
```
~U[2024-02-05 19:58:40.925931Z]
```

Um nur das aktuelle Datum zu erhalten, könnten die Komponenten für Jahr, Monat und Tag extrahiert werden:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Beispielausgabe:**
```
~D[2023-05-04]
```

### Verwendung der Timex-Bibliothek
Für komplexere Datums- und Zeitbedürfnisse kann eine beliebte Drittanbieterbibliothek namens Timex genutzt werden. Zuerst füge `Timex` zu deinen mix.exs-Abhängigkeiten hinzu:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Nachdem die Abhängigkeit installiert wurde (`mix deps.get`), kannst du Timex verwenden, um das aktuelle Datum zu erhalten:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Beispielausgabe:**
```
~D[2023-05-04]
```

Timex bietet umfassende Funktionalitäten für die Manipulation von Datums- und Zeitangaben und ist somit eine mächtige Ergänzung für deine Elixir-Anwendungen, insbesondere wenn es um Zeitzonen, Formatierung und das Parsen von Daten und Zeiten geht.

Durch das Verstehen und Nutzen der integrierten Fähigkeiten von Elixir und der Timex-Bibliothek kannst du einfach mit Daten und Zeiten in deinen Elixir-Anwendungen arbeiten, die Erfahrung entsprechend den Bedürfnissen deiner Anwendung mit Präzision und Leichtigkeit anpassen.
