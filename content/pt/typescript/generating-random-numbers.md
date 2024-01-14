---
title:                "TypeScript: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Gerar números aleatórios é uma técnica comum e útil na programação. Esses números podem ser usados para criar jogos, simulações ou até mesmo em aplicações de segurança. Também é uma ótima maneira de adicionar aleatoriedade e imprevisibilidade em seus projetos.

## Como fazer

Para gerar números aleatórios em TypeScript, podemos usar a função `Math.random()` que retorna um número decimal entre 0 e 1. Para obter um número aleatório em um intervalo específico, podemos multiplicar o valor de retorno por esse intervalo e adicionar o número mínimo desejado.

Vamos ver um exemplo:

```TypeScript
// gerar um número aleatório entre 1 e 10
const min = 1;
const max = 10;

const randomNumber = Math.random() * (max - min) + min;

console.log(randomNumber); // saída: um número aleatório entre 1 e 10
```

Também podemos usar `Math.floor()` ou `Math.ceil()` para arredondar o número para cima ou para baixo, dependendo da nossa necessidade.

## Mergulho profundo

A geração de números aleatórios é fundamentalmente baseada em algoritmos de pseudoaleatoriedade. Isso significa que os números gerados não são realmente aleatórios, mas seguem uma sequência previamente determinada. Esses algoritmos são baseados em equações matemáticas complexas que usam um valor inicial chamado de "seed". Essa semente é usada para iniciar o processo de geração de números aleatórios.

Um dos métodos mais comuns para definir uma semente é usar o valor atual do relógio do sistema. Em TypeScript, podemos obter o tempo atual em milissegundos usando `Date.now()`. Podemos então usar esse valor como a semente para nosso algoritmo de geração de números aleatórios.

É importante notar que, ao usar a função `Math.random()`, a semente é definida automaticamente e não podemos controlá-la diretamente. No entanto, existem bibliotecas e métodos mais avançados que permitem definir a semente manualmente e gerar números aleatórios mais robustos.

## Veja também 
- [Documentação do TypeScript](https://www.typescriptlang.org/docs/)
- [Explicação sobre algoritmos de geração de números aleatórios](https://www.dummies.com/programming/java/exploring-random-number-generators/)
- [Exemplos de uso da função `Math.random()`](https://www.geeksforgeeks.org/javascript-math-random-function/)