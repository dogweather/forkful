---
date: 2024-01-20 17:35:49.632031-07:00
description: "Como Faz: No come\xE7o da programa\xE7\xE3o, concatenar strings era\
  \ um processo mais verboso e menos intuitivo. Antes do ES6 em JavaScript, por exemplo,\
  \ voc\xEA teria\u2026"
lastmod: '2024-04-05T21:53:46.648247-06:00'
model: gpt-4-1106-preview
summary: "No come\xE7o da programa\xE7\xE3o, concatenar strings era um processo mais\
  \ verboso e menos intuitivo."
title: Concatenando strings
weight: 3
---

## Como Faz:
```typescript
// Usando o operador +
let saudacao = "Olá, " + "mundo!";
console.log(saudacao); // Saída: Olá, mundo!

// Com Template Strings (ou literals)
let usuario = "Ana";
let mensagem = `Bem-vinda, ${usuario}!`;
console.log(mensagem); // Saída: Bem-vinda, Ana!

// Concatenando múltiplas strings
let parte1 = "TypeScript ";
let parte2 = "é ";
let parte3 = "legal.";
let fraseCompleta = parte1 + parte2 + parte3;
console.log(fraseCompleta); // Saída: TypeScript é legal.
```

## Aprofundando
No começo da programação, concatenar strings era um processo mais verboso e menos intuitivo. Antes do ES6 em JavaScript, por exemplo, você teria que usar o operador `+` o tempo todo, o que podia causar confusão com a soma de números. Com o TypeScript, que incorpora e estende o JavaScript, obtivemos os templates strings que simplificaram esse processo.

Como alternativa, principalmente em linguagens mais antigas, temos funções específicas para concatenar, como a função `concat` do JavaScript. No TypeScript, a imutabilidade das strings significa que cada operação de concatenação resulta numa nova string, o que é algo a considerar em termos de eficiência.

A partir do ponto de vista da implementação, o compilador do TypeScript precisa gerenciar a concatenação de strings eficientemente para não ter impacto negativo no desempenho, especialmente quando manipulando strings grandes ou realizando muitas concatenações sucessivas.

## Veja Também
- Documentação oficial do TypeScript sobre template literals [Template Strings](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- Artigo sobre manipulação de strings no TypeScript/JavaScript [String Manipulation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Performance de concatenação de strings: [Concatenating Strings Efficiently](https://jsperf.com/concat-vs-plus-vs-join)
