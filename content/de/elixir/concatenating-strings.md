---
title:                "Verkettung von Zeichenketten"
html_title:           "Elixir: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Wenn du ein Programmierer bist oder gerade deine Reise in die Welt der Programmierung beginnst, hast du bestimmt schon von der Elixirsprache gehört. Elixir ist eine funktionale Programmiersprache, die auf der Erlang Virtual Machine basiert und immer mehr an Popularität gewinnt. Eine der vielen nützlichen Funktionen von Elixir ist die Fähigkeit, Strings zu concatenieren oder zusammenzufügen.

Die Verwendung von concatenierten Strings ist wichtig, um komplexe und dynamische Texte zu erstellen, wie zum Beispiel Benachrichtigungen oder Benutzeroberflächen. Durch die Kombination von Texten und Variablen können wir personalisierte und aussagekräftige Informationen erstellen. Im Folgenden schauen wir uns an, wie wir Strings in Elixir concatenieren können.

## Wie es geht

```Elixir
# Verwendung des '<>`-Operators zum Zusammenfügen von Strings
full_name = "Max" <> "Mustermann"
# Ausgabe: "Max Mustermann"

# Auch Zahlen können mit Strings concateniert werden
age = 30
full_description = "Max ist " <> age <> " Jahre alt."
# Ausgabe: "Max ist 30 Jahre alt."

# Mehrere Strings können innerhalb eines `<>`-Blocks zusammengefügt werden
greeting = "Hallo" <> " " <> "Welt"
# Ausgabe: "Hallo Welt"

# Variablen können auch in Strings interpoliert werden
username = "maxmuster"
greeting_message = "Willkommen zurück, #{username}!"
# Ausgabe: "Willkommen zurück, maxmuster!"

# Es ist auch möglich, Strings mit der `<>`-Funktion zusammenzusetzen
first_name = "Max"
last_name = "Mustermann"
full_name = String.to_charlist(first_name) <> String.to_charlist(last_name)
# Ausgabe: 'Max Mustermann'
```

Der Elixir `<>`-Operator kann sowohl mit Strings als auch mit Charlisten verwendet werden, was die Flexibilität erhöht. Wenn wir nur mit Charlisten arbeiten, können wir auch die `<>`-Funktion verwenden, um Strings zusammenzufügen.

## Tiefenblick

Für jeden String oder Charlist, die wir mit `<>` zusammenfügen, wird eine neue Kopie erstellt. Dies kann zu einem höheren Speicherverbrauch und einer geringeren Leistung führen, wenn wir eine große Anzahl von String-Concatenation in einer Schleife verwenden. In solchen Fällen kann es effizienter sein, die `Enum.join`-Funktion zu verwenden, um eine Liste von Strings effizient zusammenzuführen.

Eine weitere Möglichkeit, Strings zu concatenieren, ist die Verwendung von Elixir Bit Strings. Bit Strings sind eine effiziente Möglichkeit, binäre Daten und Unicode-Zeichen darzustellen. Wir können Bit Strings verwenden, um mehrere Strings zu concatenieren, ohne zusätzlichen Speicher zu verbrauchen.

## Siehe auch

- [Elixir Dokumentation zu Strings](https://hexdocs.pm/elixir/String.html)
- [Elixir Dokumentation zu Bit Strings](https://hexdocs.pm/elixir/BitString.html)
- [Elixir Dokumentation zu Enum](https://hexdocs.pm/elixir/Enum.html#join/2)