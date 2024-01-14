---
title:                "Elixir: Debug-Ausgabe drucken"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Debug-Ausgaben sind ein wichtiger Teil der Entwicklung von Elixir-Anwendungen. Sie ermöglichen es Entwicklern, die internen Abläufe ihres Codes besser zu verstehen und Probleme zu lösen.

# Wie man

Es gibt mehrere Möglichkeiten, Debug-Ausgaben in Elixir zu erstellen. Eine einfache Möglichkeit ist die Verwendung der `IO.inspect/2` Funktion:

```Elixir
defmodule Beispiel do
  def add(a, b) do
    IO.inspect({a, b})
    a + b
  end
end

Beispiel.add(2, 3)
```

Die `IO.inspect/2` Funktion gibt das übergebene Argument aus und gibt es auch als Rückgabewert zurück. In diesem Beispiel wird die Ausgabe `{2, 3}` sein.

Eine weitere Möglichkeit ist die Verwendung von `IO.puts/1`, um eine Nachricht direkt in die Konsole auszugeben:

```Elixir
IO.puts "Eine Debug-Nachricht"
```

Dieser Ansatz ist nützlich, wenn Sie schnell eine einfache Nachricht an der gewünschten Stelle in Ihrem Code ausgeben möchten.

# Tiefer tauchen

Es ist wichtig zu beachten, dass Debug-Ausgaben in produktionsreifen Anwendungen vermieden werden sollten, da sie die Leistung beeinträchtigen können. Stattdessen können Entwickler die `Logger`-Bibliothek verwenden, um spezifische Debug-Nachrichten zu protokollieren. Diese Nachrichten können dann mithilfe von Konfigurationsdateien gesteuert werden und werden nur angezeigt, wenn die Anwendung in einer Debug-Umgebung ausgeführt wird.

Um den Logger in Ihrer Anwendung zu verwenden, müssen Sie ihn zunächst konfigurieren, indem Sie eine `config/config.exs` Datei erstellen:

```Elixir
config :logger,
 level: :debug,
 format: "$time $metadata[$level] $message\\n",
 color: [enabled: false]
```

Dieser Code aktiviert den Logger auf dem Debug-Level und gibt eine Standardformatierung für Debug-Nachrichten an. Sie können auch spezifische Module und Funktionen angeben, für die Debug-Nachrichten protokolliert werden sollen.

Um nun den Logger in Ihren Code zu integrieren, können Sie den `Logger.debug/1` Befehl verwenden:

```Elixir
defmodule Beispiel do
  def add(a, b) do
    Logger.debug("Berechne #{a} + #{b}")
    a + b
  end
end

Beispiel.add(2, 3)
```

Diese Debug-Nachricht wird nur angezeigt, wenn der Logger auf dem Debug-Level aktiviert ist.

# Siehe auch

- Offizielle Elixir Dokumentation: https://elixir-lang.org/docs.html
- Elixir Forum: https://elixirforum.com/
- Elixir School: https://elixirschool.com/de/