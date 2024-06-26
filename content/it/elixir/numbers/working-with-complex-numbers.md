---
date: 2024-01-26 04:39:07.211310-07:00
description: "Come fare: Elixir non ha numeri complessi integrati, quindi dobbiamo\
  \ crearli noi o usare una libreria, come `ComplexNum`. Ecco un esempio rapido con\
  \ una\u2026"
lastmod: '2024-03-13T22:44:43.078129-06:00'
model: gpt-4-0125-preview
summary: Elixir non ha numeri complessi integrati, quindi dobbiamo crearli noi o usare
  una libreria, come `ComplexNum`.
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
Elixir non ha numeri complessi integrati, quindi dobbiamo crearli noi o usare una libreria, come `ComplexNum`. Ecco un esempio rapido con una libreria:

```elixir
# Supponendo che tu abbia installato ComplexNum
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Crea numeri complessi e sommali
c1 = {3, 4}   # rappresenta 3 + 4i
c2 = {2, -3}  # rappresenta 2 - 3i
risultato = ComplexMath.add(c1, c2)
IO.puts "Il risultato è: #{inspect(risultato)}"
```

Questo produrrà in output:
```
Il risultato è: {5, 1}
```

Significa che la somma di `3 + 4i` e `2 - 3i` è `5 + 1i`.

## Approfondimento
I numeri complessi sono emersi nella storia perché i semplici vecchi numeri non potevano gestire le radici quadrate dei negativi. Non furono presi sul serio fino al 17° secolo, grazie a matematici come René Descartes e Gerolamo Cardano.

In Elixir, spesso si usano tuple come `{3, 4}` per i numeri complessi, o si utilizza una lib dedicata per evitare di reinventare la ruota. Le librerie sono generalmente migliori - gestiscono dettagli complessi come moltiplicazione e divisione, che diventano complicati a causa dell'unità immaginaria 'i' (per tua informazione: `i` al quadrato è uguale a `-1`).

## Vedi Anche
Consulta queste risorse:
- [Libreria ComplexNum](https://hex.pm/packages/complex_num) per il gestore di pacchetti di Elixir, Hex.
- [Scuola di Elixir](https://elixirschool.com/en/), per argomenti avanzati di Elixir ed esercizi.
- [Erlang -- modulo math](http://erlang.org/doc/man/math.html), che Elixir utilizza sotto il cofano, per altre esigenze matematiche.
