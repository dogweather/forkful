---
title:    "Javascript: Gerando números aleatórios"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Porque

Gerar números aleatórios pode ser útil em diferentes situações, desde jogos até testes de software. Além disso, pode ser uma ótima maneira de aprender sobre lógica e programação.

## Como Fazer

Para gerar um número aleatório em Javascript, podemos utilizar a função `Math.random()`, que retorna um número entre 0 e 1. Por exemplo:

```Javascript
let num = Math.random(); // num será igual a um número aleatório entre 0 e 1
```

Para gerar um número dentro de um intervalo específico, podemos multiplicar o resultado dessa função pelo (valor máximo - valor mínimo) e adicionar o valor mínimo. Por exemplo, para gerar um número aleatório entre 1 e 100, podemos fazer:

```Javascript
let num = Math.random() * (100 - 1) + 1; // num será um número aleatório entre 1 e 100
```

Podemos também utilizar `Math.floor()` para arredondar o número para baixo, caso queiramos um número inteiro. Por exemplo:

```Javascript
let num = Math.floor(Math.random() * (100 - 1) + 1); // num será um número inteiro aleatório entre 1 e 100
```

## Mergulho Profundo

Ao gerar números aleatórios, é importante ter em mente que eles não são 100% aleatórios, mas sim pseudoaleatórios, ou seja, são gerados por um algoritmo. Isso significa que, em alguns casos, eles podem seguir um padrão e não serem completamente imprevisíveis.

Outro ponto a ser considerado é que, se a função `Math.random()` for chamada várias vezes seguidas, é possível que ela retorne os mesmos números, pois ela se baseia no relógio do sistema para gerar os números. Para evitar isso, podemos utilizar um número aleatório como semente e passá-lo como argumento para a função. Isso irá garantir que sempre teremos um número diferente gerado.

## Veja Também

- [Documentação do Math.random em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Entendendo o conceito de números aleatórios em programação](https://www.geeksforgeeks.org/generate-random-numbers-in-java/)
- [Trabalhando com números aleatórios em jogos em Javascript](https://www.w3schools.com/Js/js_random.asp)