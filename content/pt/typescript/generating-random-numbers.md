---
title:    "TypeScript: Gerando números aleatórios."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que Gerar Números Aleatórios é Importante na Programação TypeScript?

Gerar números aleatórios é uma tarefa comum na programação. Isso pode ser útil para criar jogos, simulações e testes de software. Também pode ser usado para gerar senhas ou tokens de segurança.

## Como Gerar Números Aleatórios em TypeScript

Para gerar números aleatórios em TypeScript, podemos usar a função `Math.random()`. Essa função retorna um número decimal entre 0 e 1, excluindo o 1. No entanto, para obter números aleatórios em um intervalo específico, podemos usar a fórmula `Math.random() * (max - min) + min`. Isso nos dá um número decimal entre `min` (incluindo) e `max` (excluindo).

Vamos ver um exemplo de como gerar 5 números aleatórios entre 1 e 10:

```TypeScript
for (let i = 0; i < 5; i++) {
  let randomNumber = Math.floor(Math.random() * (10 - 1) + 1);
  console.log(randomNumber);
}
```

Este código irá gerar números inteiros entre 1 e 10. Podemos modificar esse código para obter números aleatórios em outros intervalos, basta substituir os valores de `min` e `max`.

## Deep Dive: Mais Informações sobre a Geração de Números Aleatórios

Embora a função `Math.random()` possa parecer simples, é importante saber que ela não gera números verdadeiramente aleatórios. Na verdade, ela é baseada em um algoritmo que produz uma sequência de números aparentemente aleatórios. Isso significa que se usarmos a mesma semente (seed) para a função `Math.random()` em dois computadores diferentes, eles irão gerar a mesma sequência de números.

Além disso, se precisamos gerar números verdadeiramente aleatórios, devemos usar uma biblioteca de terceiros, como o [random-js](https://github.com/ckknight/random-js) ou o [faker](https://github.com/Marak/Faker.js). Essas bibliotecas usam fontes externas, como a data e hora atual ou a movimentação do mouse, para gerar números aleatórios mais precisos.

## Veja Também

- [Documentação do Math.random() em MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Exemplos de uso do random-js](https://github.com/ckknight/random-js/tree/master/examples)
- [Faker.js para gerar dados falsos](https://github.com/Marak/Faker.js)