---
title:                "String in Großbuchstaben umwandeln"
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein String wird großgeschrieben, indem der erste Buchstabe in einen Großbuchstaben umgewandelt wird. Das ist nötig für korrekte Grammatik, Namen oder Titel und erleichtert oft die Textlesbarkeit.

## So geht’s:
```elixir
defmodule StringHelper do
  def capitalize_string(s) do
    String.capitalize(s)
  end
end

# Beispielverwendung
IO.puts StringHelper.capitalize_string("elixir ist spaß")
```
Ausgabe:
```
Elixir ist spaß
```

## Vertiefung
### Historischer Kontext
Das Großschreiben von Zeichenketten ist so alt wie die Textverarbeitung selbst. Frühe Computersysteme implementierten diese Funktion für bessere Textformatierung.

### Alternativen
In Elixir gibt es `String.capitalize/1`, aber man könnte auch eigene Funktionen schreiben, z.B.:
```elixir
def my_capitalize(s) do
  [first | rest] = String.to_charlist(s)
  String.from_charlist([first |> to_upper_case] ++ rest)
end
```
### Implementierungsdetails
`String.capitalize/1` funktioniert unter der Haube mit Unicode und achtet auf Sonderzeichen. Großschreibung in Elixir ist also sprach- und kultursensitiv.

## Siehe auch
- [Elixir String Module Docs](https://hexdocs.pm/elixir/String.html)
- [Unicode Standard](http://www.unicode.org/versions/latest/)
- Weitere String-Manipulations-Funktionen in Standardbibliothek von Elixir.