---
title:                "Gerando números aleatórios"
html_title:           "Javascript: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Gerar números aleatórios é uma técnica comum usada por programadores para criar resultados aleatórios em seus códigos. Isso é útil em muitos cenários, como jogos, sorteios e testes, onde resultados imprevisíveis são necessários. Além disso, a geração de números aleatórios é uma parte importante do aprendizado de algoritmos e estruturas de dados.

## Como fazer:

```Javascript
// Usando Math.random() para gerar um número aleatório entre 0 e 1
let random = Math.random();
console.log(random); // Exemplo de saída: 0.6271100710371635

// Gerando um número aleatório inteiro entre 1 e 10
let randomInt = Math.floor(Math.random() * 10) + 1;
console.log(randomInt); // Exemplo de saída: 7
```

## Mergulho Profundo:

Historicamente, gerar números aleatórios em um computador era uma tarefa difícil. Os primeiros métodos geralmente resultavam em números pseudoaleatórios, que pareciam aleatórios, mas eram realmente previsíveis. Com o avanço da tecnologia, métodos mais robustos, como o MT19937, foram desenvolvidos para produzir números realmente aleatórios. Além disso, existem alternativas ao uso do Math.random(), como bibliotecas de terceiros, que oferecem mais opções de personalização e flexibilidade.

## Veja também:

- [MDN Web Docs - Math.random()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [W3Schools - Math.random()](https://www.w3schools.com/js/js_random.asp)
- [RandonVersity - Geradores de Números Aleatórios](https://www.randomversity.com/)