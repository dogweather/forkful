---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Gerar números aleatórios é criar valores que não podem ser preditos logicamente. Programadores fazem isso para adicionar aleatoriedade, útil em coisas como jogos, simulações e testes de software.

## Como fazer:

Aqui está como você gera números aleatórios em TypeScript.

```TypeScript
let num = Math.random(); // Gera um número aleatório entre 0 (inclusive) e 1 (exclusivo)
console.log(num); // Por exemplo: 0.9492879023357719
```

Para um número aleatório inteiro entre 0 e um valor máximo, utilize:

```TypeScript
let max = 10;
let inteiro = Math.floor(Math.random() * max);
console.log(inteiro); // Por exemplo: 7
```

## Imersão Profunda:

Historicamente, geração de números aleatórios em computação é uma abstração de fenômenos aleatórios do mundo real (por exemplo, ruído radioativo). Como alternativa em TypeScript, pode-se também usar bibliotecas externas para geração de números aleatórios, como `random-js`.

No JavaScript (e consequentemente TypeScript), `Math.random()` gera números pseudo-aleatórios usando o algoritmo XorShift128+. Isso significa que embora os números pareçam aleatórios, se você conhecer o estado interno do gerador, você pode prever os próximos números.

## Veja Também:

1. [MDN: Math.random()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
3. [Random-js (NPM)](https://www.npmjs.com/package/random-js)