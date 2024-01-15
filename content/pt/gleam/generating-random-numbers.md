---
title:                "Gerando números aleatórios"
html_title:           "Gleam: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios com Gleam?

Gerar números aleatórios é uma tarefa comum em muitos programas, seja para jogos, sorteios ou testes de desempenho. Com o uso da linguagem de programação Gleam, é possível gerar esses números de forma eficiente e confiável.

## Como fazer

Para gerar números aleatórios em Gleam, é necessário usar a biblioteca `Random` que já vem incluída na linguagem. A função `generate` dessa biblioteca permite gerar números inteiros aleatórios dentro de um intervalo específico. Veja um exemplo simples:

```
Gleam import Random
import Random

fn main() {
  let number = Random.generate(1, 100)
  IO.print("O número aleatório é $(number)")
}
```

Ao executar esse código, a saída será um número inteiro aleatório entre 1 e 100.

## Deep Dive

Agora, vamos entender melhor o processo de geração de números aleatórios em Gleam. A biblioteca `Random` utiliza um gerador de números pseudoaleatórios chamado [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister). Esse gerador é considerado um dos melhores e mais rápidos existentes na atualidade.

Além disso, a função `generate` possui uma sobrecarga que permite gerar números inteiros aleatórios com uma semente (seed) específica. Isso é útil para programas que precisam gerar sempre o mesmo número aleatório a cada execução, garantindo consistência nos resultados.

## Veja também

- [Documentação da biblioteca `Random` em Gleam](https://gleam.run/modules/standard-libraries/random/)
- [Mersenne Twister em inglês](https://en.wikipedia.org/wiki/Mersenne_Twister)