---
title:                "Zahlen runden"
aliases: - /de/elixir/rounding-numbers.md
date:                  2024-01-26T03:43:49.116126-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Runden von Zahlen bedeutet, sie zu einem nahegelegenen Wert zu justieren, um sie zu vereinfachen oder eine bestimmte Genauigkeit zu erreichen. Das ist nützlich, um die Lesbarkeit zu verbessern, Speicherplatz zu reduzieren oder domänenspezifischen Anforderungen zu entsprechen, wie bei Geldberechnungen, wo man auf den nächsten Cent runden möchte.

## Wie:
In Elixir können Sie `Float.round/2` verwenden, um eine Fließkommazahl zu runden. Sie können die Anzahl der Dezimalstellen angeben, die Sie beibehalten möchten. So funktioniert es:

```elixir
# Eine Zahl ohne Dezimalstellen runden
Float.round(3.14159) # => 3.0

# Eine Zahl auf 2 Dezimalstellen runden
Float.round(3.14159, 2) # => 3.14

# Eine Zahl mit negativer Präzision auf die nächsten 10 runden
Float.round(123.456, -1) # => 120.0
```

## Vertiefung
Das Runden von Zahlen ist ein klassisches Problem in der Informatik - so sehr, dass die Wahl der Rundungsstrategie Finanzsysteme, wissenschaftliche Berechnungen und mehr beeinflussen kann. Elixirs `Float.round/2` verwendet standardmäßig das "half up" Runden, das dem traditionellen Runden ähnelt, wie es im Mathematikunterricht gelehrt wird.

Wenn Sie andere Arten des Rundens benötigen, ermöglicht es Ihnen Elixir, eigene Methoden zu entwickeln. Betrachten Sie zum Beispiel das "floor" Runden (immer abrunden) oder das "ceiling" Runden (immer aufrunden). Dafür würden Sie `Float.floor/1` oder `Float.ceil/1` verwenden, je nach Bedarf.

```elixir
# Floor Runden
Float.floor(3.999) # => 3.0

# Ceiling Runden
Float.ceil(3.001) # => 4.0
```

Diese Alternativen helfen, das Runden an die genauen Bedürfnisse Ihrer Anwendung anzupassen, sei es für Finanzberechnungen, Grafikerstellung oder Datenannäherung.

## Siehe auch
Für mehr Informationen über Elixirs Rundungsfunktionen und Fließkommazahlen:

- Elixirs offizielle Dokumentation zu `Float`: https://hexdocs.pm/elixir/Float.html
- IEEE-Standard für Fließkommazahlen (IEEE 754): https://ieeexplore.ieee.org/document/4610935
