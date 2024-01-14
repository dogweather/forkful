---
title:                "Gleam: Geração de números aleatórios"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Gleam?

Gerar números aleatórios é uma tarefa extremamente útil em programação, especialmente em jogos, simulações e aplicações criptográficas. Em Gleam, podemos utilizar o módulo `Random` para gerar números aleatórios de forma simples e eficaz.

## Como fazer:

Podemos gerar números aleatórios em Gleam utilizando a função `int` do módulo `Random` e fornecendo os limites desejados. Por exemplo, para gerar um número aleatório entre 1 e 10, podemos utilizar o código a seguir:

```
Gleam import Random

let random_number = int(1, 10)

Random.generate(random_number)

```

Esse código irá gerar um número inteiro aleatório entre 1 e 10 e armazená-lo na variável `random_number`. Em seguida, utilizamos a função `generate` do módulo `Random` para imprimir o resultado na tela. Ao executar o código, obteremos uma saída semelhante a esta:

```
6
```

Podemos também utilizar a função `float` para gerar números aleatórios decimais. Basta fornecermos o limite inferior e superior desejado. Por exemplo:

```
let random_decimal = float(0.0, 1.0)

Random.generate(random_decimal)
```

Isso irá gerar um número decimal aleatório entre 0 e 1 e imprimi-lo na tela.

## Mergulho Profundo:

O módulo `Random` oferece diversas funções para gerar números aleatórios com diferentes tipos e limites, como por exemplo:

- `int_uniform(start, end)` para gerar um número inteiro uniformemente distribuído entre os limites fornecidos.
- `float_normal(mean, sd)` para gerar um número decimal com distribuição normal, utilizando a média e o desvio padrão desejados.
- `int_seed(seed)` para gerar um número inteiro a partir de uma semente específica.

Também é possível utilizar a função `Random.seed()` para definir uma semente padrão e garantir que a geração de números aleatórios seja sempre a mesma, útil para testes e debugging.

## Veja Também:

- Documentação oficial do módulo `Random` em Gleam: https://gleam.run/documentation/stdlib/random
- Tutorial sobre como gerar números aleatórios em Gleam: https://cleibsonalves.github.io/tutorial-gerando-numeros-aleatorios-em-gleam/#/
- Exemplos práticos de uso do módulo `Random`: https://github.com/gleam-lang/example-apps/blob/master/examples/random/random.gleam