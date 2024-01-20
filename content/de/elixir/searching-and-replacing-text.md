---
title:                "Suchen und Ersetzen von Text"
html_title:           "Bash: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen von Text ist ein häufig benötigter Prozess in der Programmierung, bei dem bestimmte Textmuster identifiziert und durch andere ersetzt werden. Programmierer tun dies, um Daten zu reinigen, zu transformieren oder zu manipulieren.

## So geht's:

Mit der Funktion `String.replace/3` in Elixir können wir Textmuster suchen und ersetzen. Hier ist ein einfaches Beispiel:

```Elixir
string = "Hallo, Welt!"
neue_string = String.replace(string, "Welt", "Elixir")
IO.puts neue_string
```

Dieses Beispiel sucht das Wort "Welt" in der String und ersetzt es durch "Elixir". Die Ausgabe ist "Hallo, Elixir!".

## Tiefer Eintauchen:

Suchen und Ersetzen von Text hat seinen Ursprung in den frühen Tagen der Textverarbeitung und hat seine Anwendungen von einfachen Texteditoren zu komplexen Programmiersprachen erweitert. In Elixir, alternatives to `String.replace/3` could be Regular expressions via `Regex.replace/3` for more complex patterns. Was die Implementierung betrifft, verwendet `String.replace/3` Intern das Erlang :binary Module, welches für seine Geschwindigkeit und Effizienz bekannt ist.

```Elixir
regex = ~r/Welt/
neue_string = Regex.replace(regex, "Hallo, Welt!", "Elixir")
IO.puts neue_string
```

Auch dieses Beispiel gibt "Hallo, Elixir!" aus, zeigt jedoch die Verwendung von regulären Ausdrücken zur Textersetzung.

## Siehe auch:

- [Elixir Dokumentation für String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [Elixir Dokumentation für Regex.replace/3](https://hexdocs.pm/elixir/Regex.html#replace/3)
- [Erlang :binary Moduldokumentation](http://erlang.org/doc/man/binary.html)