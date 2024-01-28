---
title:                "Geração de números aleatórios"
date:                  2024-01-27T20:33:24.924059-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Gerar números aleatórios em programação pode ser crítico para criar simulações, testes, criptografia e jogos. Em Gleam, é um recurso que permite aos desenvolvedores introduzir imprevisibilidade ou simular cenários do mundo real em suas aplicações.

## Como:

Para gerar números aleatórios em Gleam, você usa principalmente a biblioteca `gleam_random`. Esta biblioteca fornece funções para gerar inteiros aleatórios, floats e mais. Primeiro, certifique-se de que adicionou `gleam_random` ao seu arquivo `rebar.config` ou `mix.exs` como uma dependência.

Vamos mergulhar em alguns exemplos:

### Gerando um Inteiro Aleatório

Para produzir um inteiro aleatório dentro de um intervalo especificado, você pode usar a função `int`:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Esta função irá gerar um inteiro aleatório entre 1 e 10, inclusive.

### Gerando um Float Aleatório

Para obter um float aleatório, use a função `float`. Isso gera um float entre 0,0 e 1,0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Exemplo de Saída

Executar estas funções pode gerar saídas como:

- Para `generate_random_int()`: `5`
- Para `generate_random_float()`: `0.84372`

Lembre-se, cada execução pode levar a saídas diferentes devido à natureza da aleatoriedade.

## Aprofundamento

O módulo `gleam_random` implementa um gerador de números pseudoaleatórios (PRNG), o que essencialmente significa que os números não são verdadeiramente aleatórios, mas são difíceis de prever, emulando aleatoriedade. Os PRNGs operam começando com um valor inicial, conhecido como semente, e aplicando operações matemáticas para gerar uma sequência de números.

Historicamente, linguagens e bibliotecas implementaram vários algoritmos para PRNGs, como o Mersenne Twister ou o Gerador Linear Congruencial (LCG). A escolha do algoritmo impacta a qualidade da "aleatoriedade", com alguns sendo mais adequados para aplicações criptográficas do que outros. Enquanto a biblioteca padrão do Gleam fornece conveniência e facilidade de uso com seu módulo `gleam_random`, ela pode não ser sempre a melhor escolha para casos de uso que requerem aleatoriedade segura do ponto de vista criptográfico. Para fins criptográficos, desenvolvedores devem procurar bibliotecas especialmente projetadas para fornecer geradores de números pseudoaleatórios criptograficamente seguros (CSPRNGs), que são projetados para resistir a ataques que poderiam prever números futuros observando uma sequência de números gerados.

Em conclusão, enquanto a funcionalidade de geração de números aleatórios do Gleam é robusta para necessidades gerais de programação, aplicações com requisitos de segurança específicos devem considerar soluções criptográficas dedicadas para garantir a integridade e a segurança da sua geração de números aleatórios.
