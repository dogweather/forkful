---
title:                "Einsatz von regulären Ausdrücken"
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster zur Textsuche und -verarbeitung. Programmierer nutzen sie, um Textmuster effizient zu erkennen, extrahieren und zu manipulieren.

## How To:
Mit Elixir kannst du mit regulären Ausdrücken arbeiten, indem du das Modul `Regex` verwendest. Hier ein paar Beispiele:

### Match finden
```elixir
Regex.match?(~r/hallo/, "hallo welt!")
# Ausgabe: true
```

### Alle Vorkommen finden
```elixir
Regex.scan(~r/\d+/, "Es gibt 3 Äpfel und 5 Birnen.")
# Ausgabe: [["3"], ["5"]]
```

### Ersetzen
```elixir
Regex.replace(~r/ä/, "Kätzchen", "ae")
# Ausgabe: "Kaetzchen"
```

### Capture Groups
```elixir
Regex.run(~r/(Apache) (\d+.\d+)/, "Server-Version: Apache 2.4")
# Ausgabe: ["Apache 2.4", "Apache", "2.4"]
```

## Deep Dive
Historisch gesehen entstammen reguläre Ausdrücke der Theorie formaler Sprachen und wurden in den 1950ern konzipiert. Alternativen zu regulären Ausdrücken in Elixir sind String-Funktionen oder pattern matching, die in manchen Fällen lesbarer oder schneller sein können. Die `Regex`-Module in Elixir nutzen die PCRE-Bibliothek (Perl Compatible Regular Expressions), welche eine reichhaltige Auswahl an Pattern-Matching-Optionen bieten.

## See Also
- Elixir Dokumentation zu Regex: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Online Regex Tester und Debugger: [https://regex101.com/](https://regex101.com/)
- Einführung in Elixir Pattern Matching: [https://elixir-lang.org/getting-started/pattern-matching.html](https://elixir-lang.org/getting-started/pattern-matching.html)
