---
title:                "Javascript: Gerando números aleatórios"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Porquê
Gerar números aleatórios é uma habilidade básica e essencial na programação. Isso pode ser útil em jogos de azar, simulações, testes automatizados e muito mais.

## Como Fazer
```Javascript
// Exemplo 1: Gerando um número aleatório inteiro entre 0 e 10
let random = Math.floor(Math.random() * 11);
console.log(random); // Saída: 7

// Exemplo 2: Gerando um número aleatório decimal entre 0 e 1
let random = Math.random();
console.log(random); // Saída: 0.7852942584398025

// Exemplo 3: Gerando um número aleatório inteiro entre 50 e 100
let random = Math.floor(Math.random() * 51) + 50;
console.log(random); // Saída: 78
```

## Mergulho Profundo
Para gerar números aleatórios em JavaScript, usamos o objeto pré-definido `Math`. Ele possui dois métodos principais: `random()` e `floor()`. O método `random()` retorna um número decimal entre 0 e 1, enquanto o método `floor()` arredonda um número para o inteiro mais próximo. Usando esses dois métodos em conjunto, podemos gerar números aleatórios com diferentes intervalos e precisões.

## Veja Também
- [Tutorial: Gerando Números Aleatórios em JavaScript](https://www.freecodecamp.org/news/how-to-generate-random-numbers-in-javascript/)
- [Documentação do Objeto Math](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math)