---
title:                "Elixir: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante?

Gerar números aleatórios é uma habilidade essencial para qualquer programador, pois pode ser útil em uma variedade de cenários. Pode ser usado em jogos, simulações e até mesmo para criar senhas seguras. É uma técnica importante a ser dominada ao aprender a programar em Elixir.

## Como gerar números aleatórios em Elixir

Em Elixir, existem várias maneiras de gerar números aleatórios. A mais básica é através da função `:random.uniform/0`, que gera um número aleatório decimal entre 0 e 1.

```
Elixir
:random.uniform()
# Output: 0.7351947113512602
```

Se você quiser gerar um número inteiro aleatório, pode usar a função `:random.uniform/1` e passar o valor máximo desejado como um argumento.

```
Elixir
:random.uniform(10)
# Output: 6
```

Você também pode especificar um valor mínimo e máximo para gerar um número aleatório dentro de um intervalo específico usando a função `:random.uniform/2`.

```
Elixir
:random.uniform(1, 20)
# Output: 15
```

Além disso, você pode gerar uma lista de números aleatórios usando a função `Enum.map/2` em conjunto com `:random.uniform/0`.

```
Elixir
Enum.map(1..5, fn _ -> :random.uniform() end)
# Output: [0.37745139275592005, 0.910411885087764, 0.1531895184721482, 0.37295830162746024, 0.3396773360841482]
```

## Aprofundando-se em geração de números aleatórios

A função `:random.uniform/0` usa um algoritmo de gerador de números aleatórios bastante conhecido chamado Mersenne Twister. Ele é conhecido por produzir sequências de números com baixa correlação e alta equitabilidade.

No entanto, se você precisar de mais controle sobre a geração de números aleatórios, pode usar a biblioteca `:rand`, que fornece uma gama mais ampla de funções de geração de números aleatórios. Você pode descobrir mais sobre ela na documentação oficial do Elixir.

## Veja também

- [Documentação do Elixir sobre geração de números aleatórios](https://hexdocs.pm/elixir/1.12/Kernel.html#random_uniform/0)
- [Documentação do Elixir sobre a biblioteca :rand](https://hexdocs.pm/elixir/1.12/Random.html)