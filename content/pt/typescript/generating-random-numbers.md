---
title:    "TypeScript: Gerando números aleatórios"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em TypeScript?

Gerar números aleatórios é uma prática comum em programação, especialmente em jogos e simuladores. E em TypeScript, isso pode ser facilmente feito através da utilização de funções nativas da linguagem.

## Como fazer em TypeScript:

 ```TypeScript
 // Gerar um número inteiro aleatório entre 1 e 100
 const randomInt = Math.floor(Math.random() * 100) + 1;
 console.log(randomInt); // Output: 49

 // Gerar um número decimal aleatório entre 0 e 1
 const randomDecimal = Math.random();
 console.log(randomDecimal); // Output: 0.6458127569

 // Gerar um número aleatório entre um intervalo customizado
 function randomInRange(min, max) {
   return Math.floor(Math.random() * (max - min + 1)) + min;
 }

 console.log(randomInRange(10, 20)); // Output: 16
 ```
 
 Como podemos ver, é possível gerar números aleatórios de diferentes formas em TypeScript, utilizando os métodos `Math.floor()` e `Math.random()`.

## Profundidade na geração de números aleatórios

A geração de números aleatórios é baseada em algoritmos e pode ser afetada por diversos fatores, como a utilização de sementes (seeds) e os números pseudoaleatórios.

Uma seed é um valor inicial que determina a sequência de números aleatórios gerados pelo algoritmo. Ao utilizar a mesma seed, obteremos sempre a mesma sequência de números, o que pode ser útil para fins de testes. Em TypeScript, podemos definir uma seed através do método `Math.random()`, passando-o como argumento.

Além disso, é importante lembrar que os números aleatórios gerados por computadores são, na verdade, pseudoaleatórios. Isso significa que eles são gerados através de fórmulas matemáticas e não são totalmente aleatórios, como os eventos naturais.

## Veja também:

- [Documentação oficial do TypeScript sobre Math](https://www.typescriptlang.org/docs/handbook/standard-library.html#math)
- [Artigo sobre geração de números aleatórios em JavaScript](https://medium.com/better-programming/how-to-generate-random-numbers-in-javascript-c86274d4e54e)
- [Explicação detalhada sobre os números pseudoaleatórios](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)