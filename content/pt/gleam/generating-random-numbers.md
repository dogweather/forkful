---
title:    "Gleam: Gerando Números Aleatórios"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Gleam?

Gerar números aleatórios é uma tarefa muito comum em programação. Isso pode ser útil em vários cenários, como jogos, testes, criptografia, entre outros. Em Gleam, é muito fácil e simples gerar números aleatórios, pois a linguagem possui uma biblioteca nativa para essa função.

## Como fazer isso?

Para gerar números aleatórios em Gleam, primeiro devemos importar a biblioteca `random` usando a palavra-chave `import`. Em seguida, podemos usar a função `float` para gerar um número decimal aleatório entre 0 e 1:

```Gleam
import random

fn main() {
  random_num = random.float(0..1)
  // output: 0.465343167368
}
```

Se quisermos gerar um número inteiro aleatório, podemos usar a função `int` e especificar o intervalo em que os números devem estar:

```Gleam
import random

fn main() {
  random_int = random.int(1..10)
  // output: 5
}
```

Podemos também gerar uma lista de números aleatórios usando a função `list` e especificando o tamanho da lista e o intervalo de números:

```Gleam
import random

fn main() {
  random_list = random.list(5, 1..100)
  // output: [64, 29, 92, 18, 87]
}
```

## Aprofundando-se

A geração de números aleatórios em Gleam é baseada no gerador de números pseudoaleatórios (PRNG) `Xoshiro128+`, que é considerado um dos melhores geradores de números aleatórios atualmente. O PRNG é inicializado com uma semente gerada aleatoriamente a partir do relógio do sistema e, em seguida, gera uma sequência de números aleatórios com base nesta semente.

É importante ressaltar que os números aleatórios gerados pela função `float` são apenas uma aproximação de um número decimal, não sendo exatamente preciso. Caso precise de maior precisão, é recomendado utilizar a biblioteca `decimal` da linguagem.

## Veja também

- Documentação oficial de Gleam sobre a biblioteca random: https://gleam.run/modules/random.html
- Documentação oficial de Gleam sobre a biblioteca decimal: https://gleam.run/modules/decimal.html