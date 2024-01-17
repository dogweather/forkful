---
title:                "Gerando números aleatórios"
html_title:           "Elixir: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que e por que?

Gerar números aleatórios é o processo de produzir um número que não segue um padrão conhecido ou definido. Isso é comumente utilizado por programadores para diferentes fins, como criptografia, jogos e testes de software.

## Como fazer:

```
Elixir Math.random()
#=> 0.23546374051340545
```

Existem duas formas principais de gerar números aleatórios em Elixir. A primeira é usando o módulo `:rand` e sua função `uniform/0`, que retorna um número aleatório no intervalo de 0 a 1. A segunda opção é usar a função `random/0` do módulo Math, que também retorna um número aleatório entre 0 e 1. Ambas as opções garantem que o número gerado seja uniformemente distribuído.

## Mergulho Profundo:

Gerar números aleatórios tem sido uma preocupação constante na história da computação. Desde os primórdios, programadores tem usado diferentes algoritmos e métodos para produzir números aleatórios que atendam aos requisitos de diferentes aplicações. Além das opções mencionadas acima, Elixir também possui a biblioteca SecureRandom, que gera números criptograficamente seguros. Outra opção é a biblioteca Random, que permite controlar o seed utilizado para gerar números aleatórios.

## Veja também:

- Documentação do módulo Math em Elixir: https://hexdocs.pm/elixir/Math.html
- Documentação da biblioteca SecureRandom em Elixir: https://hexdocs.pm/elixir_secure_random/SecureRandom.html
- Documentação da biblioteca Random em Elixir: https://hexdocs.pm/elixir/Random.html