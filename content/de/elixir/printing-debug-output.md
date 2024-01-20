---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

In der Programmierung bezieht sich `Debug-Ausgabe` auf Nachrichten, die von einem Programm für Debugging-Zwecke ausgegeben werden. Es hilft Programmierern, das Verhalten ihres Codes zu verstehen und Fehler effizienter zu finden und zu beheben.

## So geht's:

In Elixir können wir die `IO.inspect/2` Funktion verwenden, um Debug-Ausgaben zu drucken. Hier ist wie:

```elixir
defmodule HelloWorld do
  def show do
    name = "Elixir"
    IO.inspect(name, label: "Debug output")
    IO.puts("Hello, #{name}")
  end
end

HelloWorld.show
```

Die Ausgabe davon würde so aussehen:

```elixir
Debug output: "Elixir"
Hello, Elixir
```

## Vertiefung:

(1) Historischer Kontext: Die Verwendung von Druckdebugging ist eine jahrzehntealte Praxis, die auch in modernen Sprachen wie Elixir in Verbindung mit weiteren, fortgeschritteneren Debugging-Tools genutzt wird.

(2) Alternativen: Im Gegensatz zum Debug-Ausgabendruck können wir auch fortgeschrittene Werkzeuge wie "IEx.pry" oder die Tracing-Funktionen in der Erlang VM verwenden.

(3) Implementierungsdetails: Die `IO.inspect/2` Funktion gibt den ersten Parameter zurück, so dass sie überall in Ihrem Code eingefügt werden kann, ohne das Verhalten zu ändern. Es kann programmiert werden, um Labels, pretty-printing und mehr zu unterstützen.

## Siehe auch:

- Erlang's Trace-Funktionen: [Erlang Trace Functions](https://erlang.org/doc/apps/runtime_tools/erlang_trace.html)
- IEx Helpers Dokumentation: [IEx Helpers Documentation](https://hexdocs.pm/iex/IEx.Helpers.html)
- Elixir's offizielle Funktionen für Debug-Ausgabe: [Elixir's IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)