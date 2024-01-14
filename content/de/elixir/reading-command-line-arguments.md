---
title:                "Elixir: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Befehlszeilenargumenten ist eine wichtige Fähigkeit in der Elixir-Programmierung. Sie ermöglicht es uns, unsere Programme auf vielseitige Weise zu nutzen, indem wir verschiedene Einstellungen und Optionen anpassen können. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man Befehlszeilenargumente in Elixir liest.

## Wie man Befehlszeilenargumente liest

Um Befehlszeilenargumente in Elixir zu lesen, nutzen wir die `System.argv` Funktion. Diese gibt uns eine Liste der an das Programm übergebenen Argumente zurück. Hier ist ein simples Beispiel, das alle Argumente ausgibt, die wir an unser Programm übergeben:

```Elixir
# greeter.exs
args = System.argv
IO.puts "Hello #{Enum.join(args, ", ")}!"
```
```sh
$ elixir greeter.exs Hugo Lillie
Hello Hugo, Lillie!
```

Das `System.argv` gibt uns eine Liste der Argumente zurück, inklusive des Namen des Programms selbst. Deshalb nutzen wir `Enum.join` um alle Argumente, außer dem ersten, miteinander zu verbinden und einen String zu bilden.

Wir können auch spezifische Argumente aus der Liste extrahieren, indem wir Indexing nutzen. Zum Beispiel können wir das zweite Argument ausgeben, in dem wir Indexing mit dem Wert 2 nutzen:

```Elixir
# greeter.exs
args = System.argv
IO.puts "Hello #{args[2]}!"
```
```sh
$ elixir greeter.exs Hugo Lillie
Hello Lillie!
```

## Deep Dive

In der Praxis gibt es oft viele verschiedene Argumente, die an ein Programm übergeben werden können. Deshalb kann es hilfreich sein, die `OptionParser` Library zu nutzen, um unsere Befehlszeilenargumente zu verarbeiten.

Die `OptionParser` Library ermöglicht es uns, den erwarteten Typ und die Anzahl von Argumenten anzugeben, die wir lesen möchten. Zum Beispiel:

```Elixir
# options.exs
extra_help = "This is some extra help text."
OptionParser.parse(args, strict: [foo: :integer, bar: :boolean], non_strict: [baz: :string], help: extra_help)
```
```sh
$ elixir options.exs --foo 42 --bar --baz "hello"
{:ok, [{:foo, 42}, {:bar, true}, {:baz, "hello"}], []}
```

Diese `OptionParser.parse` Funktion gibt uns ein Tupel zurück, das den Erfolg oder Misserfolg, die geparsten Argumente und mögliche Fehlermeldungen enthält.

## Siehe auch

 - [Elixir Dokumentation zu System.argv](https://hexdocs.pm/elixir/System.argv.html)
 - [Elixir Dokumentation zu OptionParser](https://hexdocs.pm/elixir/OptionParser.html)