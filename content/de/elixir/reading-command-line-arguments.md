---
title:    "Elixir: Lesen von Befehlszeilenargumenten"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Kommandozeilenargumenten ist eine grundlegende Fähigkeit beim Programmieren mit Elixir. Mit dieser Funktion können Benutzer die Ausführung ihres Codes beeinflussen und personalisieren. In diesem Blogbeitrag werden wir uns ansehen, warum das Lesen von Kommandozeilenargumenten für die Elixir-Entwicklung wichtig ist.

## Wie man es macht

Das Lesen von Kommandozeilenargumenten in Elixir ist relativ einfach. Wir verwenden die Funktion `System.argv/0`, die eine Liste der an das Programm übergebenen Argumente zurückgibt. Hier ist ein Beispielcode, der die Argumente ausgibt:

```
Elixir
defmodule CommandLine do
  def display_args do
    args = System.argv()
    IO.puts("Die übergebenen Argumente sind: #{args}")
  end
end
```

Um diesen Code auszuführen, speichern Sie ihn in einer Datei mit dem Namen `command_line.exs` und führen Sie ihn von der Kommandozeile mit dem Befehl `elixir command_line.exs argument1 argument2` aus. Die Ausgabe sollte folgendermaßen aussehen:

```
Die übergebenen Argumente sind: ["argument1", "argument2"]
```

Dies ist ein einfaches Beispiel, aber Sie können auch komplexe Logik in Ihre Funktionen einbauen, um auf bestimmte Argumente zu reagieren oder sie zu filtern.

## Tief eintauchen

Beim Lesen von Kommandozeilenargumenten gibt es einige wichtige Dinge zu beachten. Zunächst ist es wichtig zu wissen, dass das erste Argument in der Liste immer der Name des Programms selbst ist. Sie können dies berücksichtigen, wenn Sie eine bestimmte Aktion ausführen möchten, wenn keine anderen Argumente übergeben wurden.

Darüber hinaus können Sie mit der Funktion `System.argv/1` bestimmte Argumente aus der Liste ausfiltern. Zum Beispiel gibt `System.argv("argument1")` nur das Argument "argument1" zurück, während `System.argv(["argument1", "argument2"])` sowohl "argument1" als auch "argument2" zurückgibt.

Eine weitere wichtige Sache zu beachten ist, dass Elixir verschiedene Datentypen für argumente unterstützt, einschließlich Zahlen und boolesche Werte. Dies bedeutet, dass Sie sie direkt in Ihren Funktionen verwenden können, ohne sie umwandeln zu müssen.

## Siehe auch

- [Dokumentation zu System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [Elixir Grundlagen: Kommandozeilenargumente](https://medium.com/elixir-basic/elixir-basics-command-line-arguments-74faa2ddb334)
- [Elixir Tricks: Kommandozeilenargumente](https://hackernoon.com/elixir-tricks-command-line-arguments-6ce5b6e4d94)