---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standardfehler (stderr) ist der Datenstrom für Fehlermeldungen und Logs. Programmierer nutzen ihn, um Fehlfunktionen von normalen Outputs zu trennen, was Problemdiagnosen vereinfacht.

## How to:
Elixir benutzt den `IO`-Modul, um in stderr zu schreiben. Hier ist ein einfaches Beispiel:

```elixir
# Nachricht direkt nach stderr schreiben
IO.puts(:stderr, "Das ist ein Fehler!")

# Formatierter Text nach stderr ausgeben
IO.write(:stderr, "Fehler: #{:badarg} aufgetreten!\n")
```

Beispielausgabe im Terminal:

```
Das ist ein Fehler!
Fehler: badarg aufgetreten!
```

## Deep Dive
In UNIX-Systemen ist stderr der voreingestellte Datenstrom für Fehler seit den 1970ern. Diese Konvention ermöglicht es, stdout (Standardausgabe) und stderr in verschiedene Dateien oder Prozesse umzuleiten. In Elixir ist die Arbeit mit stderr bequem durch die `IO`-Module abstrahiert. Alternativen zu `IO.puts/2` und `IO.write/2` könnten das niedrigere-level `:erlang.format_error/1` oder das Schreiben in eine Log-Datei via einem Logging-Framework sein.

## See Also
- Elixir Documentation: [IO module](https://hexdocs.pm/elixir/IO.html)
- Erlang's :erlang module: [format_error function](http://erlang.org/doc/man/erlang.html#format_error-1)
- Elixir Logging: [Logger module](https://hexdocs.pm/logger/Logger.html)