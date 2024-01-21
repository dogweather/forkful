---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:06.973051-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Gerar números aleatórios é como jogar dados digitais: você recebe um número sem previsão. Programadores usam isso em jogos, simulações e onde mais precisarem de um elemento de surpresa ou teste.

## How to:
```gleam
import gleam/random
import gleam/io

pub fn main() {
  let seed = random.default_seed()
  let (numero_aleatorio, _seed) = random.int(seed, 1, 100)
  io.println(numero_aleatorio)
}
```

Saída de exemplo:
```
42
```

## Deep Dive
Originalmente, computadores não eram bons com o acaso; eles precisam de algo chamado "semente" para começar. No passado, usavam coisas como horário exato para isso. Agora, `gleam/random` lida com isso elegantemente, com funções que utilizam sementes implícitas ou explícitas.

Alternativas para Gleam incluem usar outras bibliotecas de linguagens como Erlang ou Elixir, que também rodam na VM do BEAM. 

Detalhes de implementação: Gleam gera números pseudoaleatórios, quer dizer, são determinísticos e reproduzíveis se você tiver a mesma semente. Para segurança ou números verdadeiramente aleatórios, precisamos de hardware especial ou APIs externas que aproveitam fenômenos físicos.

## See Also
- Gleam's standard library documentation for the `random` module: https://hexdocs.pm/gleam_stdlib/
- Erlang's `rand` module for comparison: http://erlang.org/doc/man/rand.html
- A primer on pseudo-random number generators (PRNGs): https://www.random.org/randomness/