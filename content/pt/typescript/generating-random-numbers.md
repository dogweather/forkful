---
title:                "TypeScript: Gerando números aleatórios"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que?

Gerar números aleatórios é uma tarefa comum em qualquer linguagem de programação, incluindo o TypeScript. Isso pode ser útil em uma variedade de cenários, desde jogos até testes de algoritmos.

## Como fazer

Para gerar números aleatórios em TypeScript, podemos usar o método `Math.random()` combinado com outras funções para obter o resultado desejado. Por exemplo, para gerar um número inteiro entre 1 e 10, podemos multiplicar o resultado de `Math.random()` por 10 e arredondá-lo para baixo usando `Math.floor()`. Aqui está um exemplo de código em TypeScript:

```TypeScript
// Gerando um número aleatório entre 1 e 10
let randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum); // Output: 7
```

Podemos usar essa lógica para gerar diferentes tipos de números aleatórios, como números decimais, números negativos ou até mesmo criar uma função que retorne um array com vários números aleatórios.

## Mergulho profundo

A geração de números aleatórios pode parecer simples, mas na verdade é um conceito bastante complexo. Isso porque, na verdade, não existem números verdadeiramente aleatórios em um computador. Em vez disso, os computadores usam algoritmos para simular o comportamento de um número aleatório, com base em um "seed" ou semente inicial.

Isso significa que, se usarmos a mesma semente, sempre obteremos o mesmo resultado ao gerar um número aleatório. Além disso, os números gerados por computador são considerados "pseudoaleatórios", o que significa que eles seguem um conjunto previsível de padrões matemáticos. Portanto, é importante entender os algoritmos de geração de números aleatórios e como eles podem afetar os resultados em diferentes situações.

## Veja também
- [Documentação sobre Math.random() em TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#new-known-to-devs-experimental-features)
- [Exemplos de código para gerar números aleatórios em TypeScript](https://github.com/SuriKar/RandomNumberGenerator-Typescript)
- [Artigo sobre algoritmos de geração de números aleatórios](https://en.wikipedia.org/wiki/Random_number_generation)