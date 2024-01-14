---
title:    "Elixir: Schreiben auf die Standardfehlerausgabe"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Warum

Warum sollte jemand sich mit dem Schreiben von Standardfehlern beschäftigen? Nun, beim Debuggen von Elixir Code ist es oft hilfreich, Fehlermeldungen direkt auf dem Bildschirm auszugeben, anstatt sie in einer Log-Datei zu suchen. Das Schreiben zu standard error kann dieses Prozess erleichtern und beschleunigen.

# How To

In Elixir gibt es mehrere Methoden, um zu standard error zu schreiben. Die einfachste Möglichkeit ist die Verwendung der `IO.puts/2` Funktion, die eine Nachricht an den Fehlerstrom sendet.

```Elixir
IO.puts(:stderr, "Dies ist eine Fehlermeldung.")
```

Dies wird die Nachricht "Dies ist eine Fehlermeldung." direkt auf dem Bildschirm ausgeben. Beachte, dass der erste Parameter `:stderr` ist, um anzugeben, dass die Nachricht an den Fehlerstrom gesendet werden soll.

Eine andere Möglichkeit ist die Verwendung von `IO.write/2`, um eine binäre Nachricht an den Fehlerstrom zu senden.

```Elixir
IO.write(:stderr, "Dies ist eine binäre Nachricht.")
```

Es ist auch möglich, das `:stderr`-Token zu verwenden, anstatt den Fehlerstrom explizit zu benennen.

```Elixir
IO.puts(:stderr, "Dies ist eine andere Fehlermeldung.")
```

# Deep Dive

In Elixir gibt es verschiedene Optionen für das Schreiben zu standard error. Eine Alternative zur `IO.puts/2` Funktion ist die Verwendung der `Logger.error/3` Funktion, die benutzerdefinierte Fehlermeldungen mit zusätzlichen Metadaten ausgeben kann.

```Elixir
Logger.error("Ein Fehler ist aufgetreten.", %{
  function: :my_function,
  line: 12,
  module: :my_module
})
```

Dies wird eine Fehlermeldung mit der Nachricht "Ein Fehler ist aufgetreten." und den zusätzlichen Metadaten auf dem Bildschirm ausgeben.

Man muss jedoch auch beachten, dass das Schreiben zu standard error nicht immer die beste Wahl ist. Wenn es darum geht, Fehler in Produktionsumgebungen zu beheben, ist es oft besser, eine Log-Datei zu verwenden, um alle Fehler an einem zentralen Ort zu erfassen und zu überwachen.

# Siehe auch

- Offizielle Elixir-Dokumentation zu `IO`
- Artikel "Debugging in Elixir" von Elixir School
- Blogbeitrag "Fehlerbehandlung in Elixir" von DevInsider