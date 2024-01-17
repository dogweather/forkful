---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "Elixir: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum? 

Die Umwandlung von Groß- zu Kleinschreibung ist ein häufiges Problem beim Programmieren von Textverarbeitungsanwendungen. Oftmals benötigen wir alle Buchstaben eines Textes in einer bestimmten Schreibweise, sei es Groß- oder Kleinschreibung. Durch die Umwandlung in Kleinschreibung wird es einfacher, Suchvorgänge und Textvergleiche durchzuführen.

## Wie geht's?: 

Kleinschreibung ist in Elixir ganz einfach. Verwenden Sie einfach die Funktion `String.downcase()` und geben Sie den zu konvertierenden String als Argument ein. Schauen wir uns ein Beispiel an:

```elixir
iex> String.downcase("ELIXIR")
"elixir"
```

Wie Sie sehen können, wird der String "ELIXIR" in "elixir" umgewandelt. Wenn der String bereits in Kleinschreibung vorliegt, bleibt er unverändert.

```elixir
iex> String.downcase("elixir")
"elixir"
```

## Tief eintauchen: 

Die Konvertierung von Groß- zu Kleinschreibung gibt es schon seit langem in der Programmierung und wird unter anderem für die Erstellung von Texteditor-Programmen verwendet. Es gibt auch alternative Möglichkeiten, String-Änderungen in Elixir durchzuführen, wie zum Beispiel den `String.upcase()` und `String.capitalize()` Funktionen. Diese ermöglichen es, den Text in umgekehrter Schreibweise oder nur den ersten Buchstaben in Großbuchstaben zu ändern.

Die Funktion `String.downcase()` verwendet intern die `String.chardowncase/1` Funktion, um jeden einzelnen Buchstaben des Strings in Kleinschreibung umzuwandeln.

## Siehe auch: 

- [Elixir String Modul Dokumentation](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Blog-Post: String-Änderungen in Elixir](https://parkour.org/blog/converting-strings-in-elixir/)