---
title:                "Großschreibung eines Strings"
html_title:           "Elixir: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

"## Was & Warum?"

Das Kapitalisieren eines Strings bedeutet, den ersten Buchstaben jedes Wortes in einem Satz in Großbuchstaben zu schreiben, während alle anderen Buchstaben klein bleiben. Programmierer tun dies, um die Lesbarkeit und Ästhetik von Texten in ihrer Codebasis zu verbessern.

"## Wie geht's:"

```elixir
string = "hallo, elixir!"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```
```elixir
# Output: Hallo, elixir!
```

"## Tiefenanalyse:"

Das Kapitalisieren von Strings wurde bereits in frühen Programmiersprachen wie BASIC verwendet, um menschenlesbaren Code zu schaffen. In einigen Sprachen gibt es auch spezielle Funktionen wie "titlecase", um den ersten Buchstaben jedes Wortes in einem String zu kapitalisieren. In Elixir gibt es die String.capitalize/1 Funktion, die das erste Zeichen in einem String in Großbuchstaben umwandelt.

"## Siehe auch:"

Weitere Informationen zur String.capitalize/1 Funktion finden Sie in der offiziellen Elixir-Dokumentation: https://hexdocs.pm/elixir/String.html#capitalize/1