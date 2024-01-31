---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:56:12.750245-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente sind Infos, die du deinem Elixir-Programm beim Start übergibst. Wir nutzen sie, um das Verhalten unserer Programme dynamisch anzupassen, ohne den Code zu ändern.

## How to:
In Elixir greifst du mit `System.argv()` auf die Argumente zu. Einfaches Beispiel:

```elixir
defmodule CLIExample do
  def main do
    args = System.argv()
    IO.inspect(args)
  end
end

CLIExample.main()
```

Führe das Programm so aus: `elixir script.exs arg1 arg2 arg3`
Ausgabe: `["arg1", "arg2", "arg3"]`

Möchtest du Argumente umwandeln oder Default-Werte setzen? Schau dir `OptionParser.parse/2` an:

```elixir
defmodule CLIExample do
  def main do
    {options, _, _} = OptionParser.parse(System.argv(), switches: [name: :string])

    case options do
      [name: name] -> IO.puts("Hallo, #{name}!")
      _ -> IO.puts("Kein Name angegeben.")
    end
  end
end

CLIExample.main()
```

Führe das Programm so aus: `elixir script.exs --name Hans`
Ausgabe: `Hallo, Hans!`

## Deep Dive
Elixir's Kommandozeilenargumente sind nicht revolutionär. Sie machen, was sie sollen - ähnlich wie in anderen Sprachen. Doch mit Pattern Matching und der robusten `OptionParser`-Modul bietet Elixir einen eleganten Weg, Argumente zu handhaben.

Früher, in Sprachen wie C, musstest du oft mühsam die `argv` Variable manuell parsen. Aber heutzutage bieten die meisten Sprachen, auch Elixir, komfortable Lösungen für so alltägliche Aufgaben.

Es gibt Alternativen zu `System.argv()` und `OptionParser.parse/2`, wie z. B. das Einbinden von externen Bibliotheken für komplexere CLI-Anwendungen. Diese bieten oft zusätzliche Features wie automatische Hilfe-Seiten oder Befehlsvalidierungen.

Elixir selbst kommt mit mächtigen Werkzeugen für Kommandozeilenoptionen, die in vielen Fällen völlig ausreichen. Und dank Elixir's konsequentem Functional-Programming-Ansatz sind diese Werkzeuge oft überraschend einfach in der Anwendung.

## See Also
- Elixir Dokumentation zu `System.argv()`: https://hexdocs.pm/elixir/System.html#argv/0
- Elixir Dokumentation zu `OptionParser`: https://hexdocs.pm/elixir/OptionParser.html
- Mix Dokumentation für eigene Tasks: https://hexdocs.pm/mix/Mix.Task.html

Mit diesen Ressourcen kannst du tiefer in die Materie eintauchen und deine Skills im Umgang mit Kommandozeilenargumenten in Elixir verbessern.
