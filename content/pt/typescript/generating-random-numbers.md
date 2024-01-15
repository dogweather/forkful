---
title:                "Geração de números aleatórios"
html_title:           "TypeScript: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que

Gerar números aleatórios é uma funcionalidade essencial em muitas aplicações de software. Pode ser usado para criar senhas seguras, gerar dados de teste para programas e muito mais. Além disso, é uma ótima maneira de adicionar aleatoriedade e imprevisibilidade a um programa.

## Como fazer

```TypeScript
// Importando a função de geração de números aleatórios do módulo Math
import { random } from "math"

// Gerando um número inteiro aleatório de 0 a 10
let num = Math.floor(random() * 11)

// Imprimindo o número gerado no console
console.log(num)
```

Saída de exemplo: 7

```TypeScript
// Gerando um número decimal aleatório entre 0 e 1
let num = Math.random()

// Imprimindo o número gerado no console
console.log(num)
```

Saída de exemplo: 0.628195612641899

## Profundidade

A geração de números aleatórios pode parecer simples, mas há algumas coisas importantes a serem consideradas ao usá-la em seus programas. A função `random()` do módulo Math retorna um número decimal entre 0 e 1, mas esse número pode ser multiplicado e arredondado para gerar diferentes tipos de números aleatórios, como no exemplo acima. Além disso, é importante ter em mente que os números gerados são pseudoaleatórios, o que significa que são criados a partir de um algoritmo matemático e não são verdadeiramente "aleatórios" no sentido estrito da palavra. Isso pode ser um problema ao lidar com questões de segurança, por exemplo.

## Veja também

- [Documentação do módulo Math do TypeScript](https://www.typescriptlang.org/docs/handbook/standard-library.html#math)
- [Artigo "Random Numbers in JavaScript" (em inglês)](https://medium.com/@nitinpatel_20236/random-numbers-in-javascript-67d5a6935d4c)
- [Vídeo "Generating Random Numbers in TypeScript" (em inglês)](https://www.youtube.com/watch?v=XNbJu7OELDw)