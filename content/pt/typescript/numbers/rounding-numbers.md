---
date: 2024-01-26 03:46:57.107413-07:00
description: "Como fazer: O arredondamento em TypeScript pode ser feito usando v\xE1\
  rios m\xE9todos. Aqui est\xE1 uma r\xE1pida explica\xE7\xE3o."
lastmod: '2024-03-13T22:44:46.320846-06:00'
model: gpt-4-0125-preview
summary: "O arredondamento em TypeScript pode ser feito usando v\xE1rios m\xE9todos."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como fazer:
O arredondamento em TypeScript pode ser feito usando vários métodos. Aqui está uma rápida explicação:

```typescript
// Math.round arredonda para o inteiro mais próximo
console.log(Math.round(1.5)); // Saída: 2

// Math.ceil arredonda para cima para o inteiro mais próximo
console.log(Math.ceil(1.1)); // Saída: 2

// Math.floor arredonda para baixo para o inteiro mais próximo
console.log(Math.floor(1.8)); // Saída: 1

// toFixed arredonda para um número fixo de casas decimais
let num = 1.23456;
console.log(num.toFixed(2)); // Saída: "1.23"
// Nota: toFixed retorna uma string! Use parseFloat para converter de volta se necessário.
console.log(parseFloat(num.toFixed(2))); // Saída: 1.23
```

## Aprofundando
No passado, o arredondamento era uma necessidade devido ao espaço limitado e problemas de precisão em computadores antigos. Hoje, a aritmética de ponto flutuante pode levar a resultados peculiares devido à forma como os números são armazenados em binário. Alternativas para arredondamento incluem floor, ceil e trunc (para cortar decimais sem arredondar).

Vale a pena notar os detalhes internos: `Math.round` segue o "arredondamento para meio acima" (também conhecido como "arredondamento comercial"), enquanto `Math.floor` e `Math.ceil` são diretos. `toFixed` pode causar resultados inesperados porque retorna uma string, e ele arredonda usando o "arredondamento da metade para o par" (também conhecido como "arredondamento do banqueiro"), especialmente útil para reduzir viés no arredondamento dos mesmos números várias vezes.

## Veja também
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [Padrão IEEE para Aritmética de Ponto Flutuante (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
