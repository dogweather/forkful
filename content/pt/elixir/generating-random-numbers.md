---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

---

## O que & Por quê?

Gerar números aleatórios é a produção de uma sequência de números que não possuem nenhum padrão previsível. É uma tarefa comum na programação quando requeremos elementos de imprevisibilidade, como na simulação de eventos aleatórios ou geração de senhas seguras.

---

## Como fazer:

Vamos ver como gerar números aleatórios em Elixir. Aqui está um exemplo simples usando a função `:rand.uniform/1`:

```elixir
IO.inspect(:rand.uniform(10))  # gera um número aleatório entre 1 e 10
```

Este código vai retornar um número aleatório entre 1 e 10. Você pode fornecer diferentes argumentos para a função uniforme para gerar números aleatórios em um intervalo diferente. Para números aleatórios entre intervalos específicos, você pode usar:

```elixir
num = :rand.uniform(10) + 20   # gera um número aleatório entre 20 e 30
IO.inspect(num)
```

---

## Mergulho Profundo:

Historicamente, no início da programação de computadores, a geração de números aleatórios era uma tarefa complexa. Tínhamos que depender de fontes externas, como o ruído branco de um equipamento eletrônico.

Elixir, como Erlang, usa o módulo `:rand` para geração de números aleatórios. Inicialmente, Erlang usava o algoritmo 'random', que era menos preciso, mas a partir do OTP 18, foi substituído pelo módulo `:rand`, fornecendo geração de números aleatórios suficientemente boa para a maioria dos propósitos.

Existem alternativas à função `:rand.uniform/n`, como, por exemplo, a função `:random.uniform/n`, que foi desaprovada no OTP 18.0 para a geração de números aleatórios, mas ainda é utilizada por alguns por razões de compatibilidade para trás.

Geração de números aleatórios em Elixir é baseada em semente. Isso significa que você precisa semear o gerador de números aleatórios para produzir diferentes números cada vez que seu programa é executado. Use `:rand.seed(:exsplus, {m, n, x})` para definir uma semente única.

---

## Veja Também:

1. [Documentação do Elixir](https://hexdocs.pm/elixir/String.html): Explore o poderoso e elegante Elixir, com foco em sua simplicidade e alta produtividade.

2. [REPL do Elixir](https://elixir-repl.com/): Pratique Elixir em tempo real sem instalar nada no seu sistema.

3. [Exercism Elixir Track](https://exercism.io/tracks/elixir): Aprenda Elixir através de exercícios práticos e obtenha feedback de mentores.

4. [Documentação OTP](http://erlang.org/doc/apps/stdlib/rand.html): Aprofunde-se na biblioteca padrão do Erlang/OTP, usada extensivamente em Elixir.