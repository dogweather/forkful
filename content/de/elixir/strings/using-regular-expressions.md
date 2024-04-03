---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:29.192011-07:00
description: "Wie geht das: Elixir verwendet das `Regex`-Modul, welches die regex-Bibliothek\
  \ von Erlang nutzt, f\xFCr regex-Operationen. Hier sind grundlegende Anwendungen."
lastmod: '2024-03-13T22:44:53.452644-06:00'
model: gpt-4-0125-preview
summary: "Elixir verwendet das `Regex`-Modul, welches die regex-Bibliothek von Erlang\
  \ nutzt, f\xFCr regex-Operationen."
title: "Regul\xE4re Ausdr\xFCcke verwenden"
weight: 11
---

## Wie geht das:
Elixir verwendet das `Regex`-Modul, welches die regex-Bibliothek von Erlang nutzt, für regex-Operationen. Hier sind grundlegende Anwendungen:

```elixir
# Einem Muster entsprechen - Gibt die erste Übereinstimmung zurück
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Ausgabe: ["hello"]

# Alle Übereinstimmungen finden
all_matches = Regex.scan(~r/\d/, "Es gibt 2 Äpfel und 5 Orangen.")
IO.inspect(all_matches) # Ausgabe: [["2"], ["5"]]

# Teile eines Strings ersetzen
replaced_string = Regex.replace(~r/\s+/, "Elixir macht Spaß", "_")
IO.inspect(replaced_string) # Ausgabe: "Elixir_macht_Spaß"
```

Für komplexere Muster und Funktionalitäten könnte man in Erwägung ziehen, Drittanbieter-Bibliotheken zu verwenden, obwohl für die meisten grundlegenden Zeichenketten- und Musterabgleichsaufgaben, das eingebaute `Regex`-Modul von Elixir ziemlich mächtig ist.

Um eine Groß-/Kleinschreibung unabhängige Übereinstimmung zu durchführen, verwendet man die Option `i`:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Ausgabe: ["Hello"]
```

Regex-Ausdrücke können vorab kompiliert werden, um Effizienz zu erzielen, wenn sie mehrmals verwendet werden:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Ausgabe: ["hello"]
```

Elixir unterstützt auch benannte Erfassungen, welche sehr praktisch sein können, um spezifische Teile einer Zeichenkette zu extrahieren und dabei Ihren Code lesbarer zu machen:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Ausgabe: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Diese kurze Übersicht unterstreicht, wie einfach Elixir den Umgang mit regulären Ausdrücken handhabt und ermöglicht leistungsstarke Techniken zur Zeichenkettenmanipulation und Datengewinnung.
