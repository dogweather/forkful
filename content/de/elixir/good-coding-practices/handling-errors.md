---
date: 2024-01-26 00:51:04.460534-07:00
description: "Fehler zu behandeln bedeutet, Code zu schreiben, der damit umgehen kann,\
  \ wenn etwas schiefgeht. Programmierer tun das, um Abst\xFCrze zu verhindern und\
  \ um\u2026"
lastmod: '2024-03-11T00:14:27.435001-06:00'
model: gpt-4-1106-preview
summary: "Fehler zu behandeln bedeutet, Code zu schreiben, der damit umgehen kann,\
  \ wenn etwas schiefgeht. Programmierer tun das, um Abst\xFCrze zu verhindern und\
  \ um\u2026"
title: Fehlerbehandlung
---

{{< edit_this_page >}}

## Was & Warum?

Fehler zu behandeln bedeutet, Code zu schreiben, der damit umgehen kann, wenn etwas schiefgeht. Programmierer tun das, um Abstürze zu verhindern und um sicherzustellen, dass ihre Programme sich anmutig erholen können, wenn das Murphys Gesetz zuschlägt.

## Wie geht das:

In Elixir verwenden wir oft Pattern Matching und die `case`-Anweisung, um verschiedene Ergebnisse zu behandeln, einschließlich Fehler.

```elixir
defmodule Beispiel do
  def teilen(a, b) do
    case b do
      0 -> {:error, "Division durch Null nicht möglich."}
      _ -> {:ok, a / b}
    end
  end
end

# Erfolgreiche Division
{:ok, ergebnis} = Beispiel.teilen(10, 2)
IO.puts("10 / 2 ist #{ergebnis}")

# Versuch, durch Null zu teilen
{:error, grund} = Beispiel.teilen(10, 0)
IO.puts("Fehler: #{grund}")
```

Beispielausgabe:
```
10 / 2 ist 5.0
Fehler: Division durch Null nicht möglich.
```

Wenn Sie diesen Elixir-Code ausführen, erhalten Sie je nach Eingabe entweder eine erfolgreiche Division oder eine Fehlermeldung. Keine Abstürze hier!

## Vertiefung

Früher war die Fehlerbehandlung oft mit der Überprüfung von Rückgabewerten verbunden. Mit den funktionalen Wurzeln von Elixir haben wir jedoch Pattern Matching und getagte Tupel, wie `{:ok, wert}` oder `{:error, grund}`, die eleganter sind.

Es gibt andere Wege, um Fehler in Elixir zu behandeln:

- **Elixirs `try` und `rescue`**, die dem traditionellen `try-catch` in imperativen Sprachen ähneln, aber seltener verwendet werden wegen Elixirs Vorliebe für Explizitheit.
- **Supervisoren und GenServer**, Teil des OTP-Frameworks von Elixir, die mehr mit Fehlertoleranz zu tun haben. Sie beobachten den Prozess Ihres Codes und sind bereit, ihn neu zu starten, wenn etwas schiefgeht.

In der Implementierung baut Elixir auf die Robustheit von Erlang auf. Fehler werden behandelt wie jede andere Art von Nachricht, mit all dem Pattern Matching und funktionalen Feinheiten.

## Siehe auch

Für weitere Informationen zur Fehlerbehandlung in Elixir, siehe:

- Elixirs offizieller Leitfaden zur [Fehlerbehandlung](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Erfahren Sie mehr über [Prozesse und OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- Das Elixir Forum ist immer ein guter Ort, um Fragen zu stellen: [https://elixirforum.com](https://elixirforum.com).
