---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Gerar números aleatórios com Javascript nos permite criar dados imprevisíveis. Programadores fazem isso para testes, jogos, segurança e muitos outros usos.

## Como fazer:

Aqui está um exemplo de como você pode gerar um número aleatório em Javascript. 

```javascript
// Gera um número aleatório entre 1 e 100
var aleatorio = Math.floor(Math.random() * 100) + 1;  
console.log(aleatorio);
```

A cada vez que você executa este código, você verá um output diferente cada vez, isso porque o número gerado é sempre aleatório.

## Mergulho Profundo:

A função Math.random() tem sido uma parte integrante do JavaScript desde a sua criação. Gera um número flutuante (entre 0 e 1) e para obter um número inteiro aleatório, utilizamos a função Math.floor(). 

Existem outras formas de gerar números aleatórios, como o Crypto.getRandomValues para uma geração mais segura. Este é geralmente utilizado em situações onde a segurança é um fator primordial, por exemplo, na geração de tokens de autenticação.

O JavaScript usa uma semeadura interna para gerar números aleatórios (como todos os algoritmos de geração de números pseudoaleatórios), então, se você precisa de repetibilidade você pode querer olhar para outras alternativas que permitem a semeadura personalizada.

## Veja também:

- Math.random() na MDN: [https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- Crypto.getRandomValues() na MDN: [https://developer.mozilla.org/pt-BR/docs/Web/API/Crypto/getRandomValues](https://developer.mozilla.org/pt-BR/docs/Web/API/Crypto/getRandomValues)
- Deterministic (seedable) Javascript random number generator for node.js and the browser, Seedrandom.js: [https://github.com/davidbau/seedrandom](https://github.com/davidbau/seedrandom)