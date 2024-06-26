---
date: 2024-01-20 17:52:10.952463-07:00
description: "How to: Mit Elixir kannst du einfach debuggen. Nutze die `IO.inspect/2`\
  \ Funktion, um Werte w\xE4hrend des Programmablaufs zu \xFCberpr\xFCfen."
lastmod: '2024-03-13T22:44:53.535914-06:00'
model: gpt-4-1106-preview
summary: Mit Elixir kannst du einfach debuggen.
title: Debug-Ausgaben drucken
weight: 33
---

## How to:
Mit Elixir kannst du einfach debuggen. Nutze die `IO.inspect/2` Funktion, um Werte während des Programmablaufs zu überprüfen.

```elixir
# Einfaches Beispiel
defmodule MyModule do
  def greet(name) do
    IO.inspect(name, label: "Der empfangene Name")
    "Hallo #{name}"
  end
end

MyModule.greet("Welt")
```

Ausgabe im Terminal könnte so aussehen:
```
Der empfangene Name: "Welt"
"Hallo Welt"
```

## Deep Dive
Elixir basiert auf der Erlang VM (BEAM), daher sind die Debug-Funktionen teilweise von Erlangs leistungsstarken Tracing-Fähigkeiten geerbt. `IO.inspect/2` ist für den Einstieg praktisch, aber für komplexeren Debugging solltest du in Tools wie `:observer.start` oder Debugging-Packages wie `:debugger` einsteigen.

Alternativ zum direkten Ausdrucken gibt es auch strukturiertere Logging-Frameworks wie `Logger`, die mehr Kontrolle und Flexibilität bieten.

Der Einsatz von `IO.inspect/2` kann eleganter gestaltet werden durch Verwendung in einer Pipe:
```elixir
"foo"
|> IO.inspect(label: "vor Änderung")
|> String.upcase()
|> IO.inspect(label: "nach Änderung")
```

Das liefert dann eine klare Trennung des Datenflusses und des Debugging:

```
vor Änderung: "foo"
nach Änderung: "FOO"
```

## See Also
- Elixir's `IO` Modul: https://hexdocs.pm/elixir/IO.html
- Erlang's `:observer` Tool: http://erlang.org/doc/man/observer.html
- Elixir's `Logger`: https://hexdocs.pm/logger/Logger.html
- Elixir Forum für Diskussionen und Fragen: https://elixirforum.com
