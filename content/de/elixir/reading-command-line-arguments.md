---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Kommandozeilenargumenten ermöglicht die Interaktion des Benutzers mit deinem Programm über die Kommandozeile. Entwickler nutzen diese Funktion, um flexible Programme zu erstellen, die unterschiedliche Aktionen basierend auf den eingegebenen Argumenten ausführen.

## Wie macht man das:

Es ist sehr einfach, Kommandozeilenargumenten in Elixir zu verwenden. Betrachten wir die folgenden Codeschnipsel:

```Elixir
defmodule Hello do
  def main(args) do
    args
    |> Enum.join(" ")
    |> IO.puts()
  end
end

System.argv |> Hello.main
```
Wenn Sie das obige Programm mit Argumenten aus der Befehlszeile ausführen, z. B. `elixir hello.exs Hallo Welt`, gibt es `Hallo Welt` auf dem Bildschirm aus.

## Vertiefung 

Elixir basiert auf der Erlang-VM, einem System, das ursprünglich für robuste Telekommunikationssysteme entwickelt wurde. Daher bietet es starken Support für Kommandozeilenanwendungen, einschließlich des Zugriffs auf Kommandozeilenargumente.

Ein Alternativansatz zum Lesen von Befehlszeilenargumenten ist die Verwendung der Funktion `:init.get_plain_arguments/0`. Diese Funktion gibt Ihnen direkten Zugriff auf die Argumentenliste, die der Erlang-VM beim Start übergeben wird.

Die Implementierung der Argumentenlesung in Elixir erfolgt durch das `System.argv` Modul. Dieses Modul bietet Funktionen zum Abrufen und Ändern der aktuellen Argumentenliste des Prozesses.

## Siehe auch

- [Elixir School: Command line](https://elixirschool.com/de/lessons/basics/mix/#command-line)
- [Official Elixir docs: System.argv](https://hexdocs.pm/elixir/System.html#argv/0)
- [Erlang docs: init.get_plain_arguments](http://erlang.org/doc/man/init.html#get_plain_arguments-0)