---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:50:15.185889-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Gerar números aleatórios é como jogar dados digitalmente - você obtém um valor imprevisível cada vez que executa a função. Programadores fazem isso para tudo: desde simples sorteios até complexos algoritmos de criptografia.

## Como Fazer:
```TypeScript
// Exemplo Simples para Gerar um Número Aleatório entre 0 (inclusive) e 1 (exclusivo)
let aleatorio = Math.random();
console.log(aleatorio);

// Gerar um Número Inteiro Aleatório entre dois valores, min (inclusivo) e max (exclusivo)
function gerarInteiroAleatorio(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min) + min);
}

// Uso da função
console.log(gerarInteiroAleatorio(1, 10)); // Exemplo de resultado: 7
```

## Mergulho Profundo
Antigamente, números aleatórios eram gerados manualmente ou através de dispositivos mecânicos. No mundo da programação, usamos algoritmos chamados geradores de números pseudoaleatórios (PRNGs - Pseudo-Random Number Generators), pois os computadores não conseguem criar verdadeira aleatoriedade sem uma fonte externa de imprevisibilidade.

Uma alternativa moderna é usar o `crypto.getRandomValues()` que é uma função criptograficamente segura para gerar números aleatórios, disponível no ambiente do navegador e no Node.js.

```TypeScript
// Uso de crypto.getRandomValues para Números Aleatórios em TypeScript
const buffer = new Uint32Array(1);
window.crypto.getRandomValues(buffer);
console.log(buffer[0]); // Saída: um grande número inteiro aleatório
```

Detalhe importante: enquanto `Math.random()` é suficiente para casos de uso comuns não seguros, para qualquer coisa que exija robustez em segurança, como criptografia, sempre opte por abordagens como `crypto.getRandomValues()`.

## Veja Também
- [MDN Web Docs - Math.random()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web Docs - Crypto.getRandomValues()](https://developer.mozilla.org/pt-BR/docs/Web/API/Crypto/getRandomValues)
- [Node.js crypto module documentation](https://nodejs.org/api/crypto.html)