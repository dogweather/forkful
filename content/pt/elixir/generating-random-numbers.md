---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:48:52.530612-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Gerar números aleatórios é o processo de criar uma sequência de números não preditíveis. Programadores fazem isso por várias razões, como para jogos, simulações ou para segurança em criptografia.

## How to:
Elixir possui uma biblioteca embutida chamada `:rand` que fornece funções para números aleatórios. Aqui está como usá-la:

```elixir
# Para gerar um número aleatório:
random_number = :rand.uniform()
IO.puts(random_number)

# Para gerar um número entre 1 e 10:
random_number_1_to_10 = :rand.uniform(10)
IO.puts(random_number_1_to_10)
```

Exemplo de saída:
```
0.4435846175944661
7
```

## Deep Dive
Elixir usa Erlang's `:random` e `:rand` módulos para gerar números aleatórios. No passado, `:random` era utilizado, mas foi substituído pelo mais novo e melhor `:rand`. Este possui melhor qualidade na distribuição de números e é mais eficiente. Como alternativas, existem bibliotecas de terceiros como `ex_rand` para mais funcionalidades.

O módulo `:rand` do Erlang implementa algoritmos como Mersenne Twister, que é conhecido por sua alta periodicidade e uniformidade. Importante destacar que esses números não são seguros para criptografia pois são determinísticos se a semente for conhecida. Para fins de segurança, você precisará de algo como a biblioteca `crypto`, que gera números adequados para criptografia.

## See Also
- [Erlang :rand documentation](http://erlang.org/doc/man/rand.html)
- [Hex.pm para encontrar bibliotecas Elixir](https://hex.pm/)
- [Elixir School para tutoriais básicos de Elixir](https://elixirschool.com/pt/)
