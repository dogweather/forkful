---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:48:51.930768-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Cos'è e Perché?)
Generare numeri casuali significa produrre valori non previsti. Programmatori lo fanno per test, giochi, simulazioni e crittografia.

## How to: (Come Fare:)
```elixir
# Installa la libreria randex
defp deps do
  [
    {:randex, "~> 2.0"}
  ]
end

# Generazione di un numero casuale tra 0 e 100
random_num = :rand.uniform(100)
IO.puts(random_num)

# Uso di Randex per un numero casuale
Randex.stream() |> Enum.take(1) |> IO.inspect()
```

Output potrebbe essere:
```
42
[95710212]
```

## Deep Dive (Approfondimento)
La funzione `:rand.uniform/1` di Erlang è utilizzata in Elixir per numeri casuali. Prima di `:rand`, c'era `:random` che è deprecata. La libreria `randex` fornisce funzionalità aggiuntive.

Generare numeri veramente casuali è difficile per i computer che seguono istruzioni definite. In pratica, se si predispone lo stesso "seed", i numeri generati saranno gli stessi. In contesti di crittografia, è fondamentale che i numeri siano imprevedibili, quindi si usano generatori di numeri casuali crittograficamente sicuri (CSPRNG).

## See Also (Vedi Anche)
- [Randex Library Hex Package](https://hex.pm/packages/randex)