---
date: 2024-01-26 03:45:34.382177-07:00
description: "Arredondar \xE9 eliminar o ru\xEDdo ap\xF3s um determinado ponto em\
  \ um n\xFAmero. Programadores arredondam para controlar a precis\xE3o, gerenciar\
  \ mem\xF3ria ou tornar a\u2026"
lastmod: '2024-03-13T22:44:46.957574-06:00'
model: gpt-4-0125-preview
summary: "Arredondar \xE9 eliminar o ru\xEDdo ap\xF3s um determinado ponto em um n\xFA\
  mero."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como fazer:
Aqui está como você arredonda números em JavaScript usando `Math.round()`, `Math.ceil()`, e `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (já que .567 é mais do que .5)

console.log(roundedDown); // Imprime: 2
console.log(roundedUp);   // Imprime: 3
console.log(rounded);     // Imprime: 3
```

Para fixar em um certo número de casas decimais, use `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (retorna uma string)

console.log(twoDecimals); // Imprime: "2.57"
```

Converta a string de volta para um número com um mais unário ou `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Imprime: 2.57
```

## Mergulho Profundo
Arredondar números não é novo; é tão antigo quanto os números. Em JavaScript, `Math.round()` usa o "arredondamento para metade mais próxima": se a parte fracionária é 0,5, ele arredonda para o número par mais próximo.

Para mais controle, `toFixed()` pode ser sua primeira escolha, mas lembre-se, ele retorna uma string. Converter de volta para um número pode ser um passo extra, mas garante que você continue trabalhando com tipos numéricos.

Alternativas? Bibliotecas como `lodash` oferecem `_.round(number, [precision=0])` para um controle mais matizado. Ou, o mais novo `Intl.NumberFormat` lhe dá formatação de alta precisão além de apenas arredondar.

Falando em precisão, cuidado com as peculiaridades de ponto flutuante em JavaScript. `0.1 + 0.2` não é exatamente igual a `0.3` devido a como os números são armazenados. Às vezes, arredondar torna-se necessário para corrigir tais erros de ponto flutuante.

## Veja Também
- Documentação Math da Mozilla: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Arredondamento financeiro com `Intl.NumberFormat`: [API de Internacionalização ECMAScript](https://tc39.es/ecma402/#numberformat-objects)
- Arredondamento `lodash`: [Documentos Lodash](https://lodash.com/docs/4.17.15#round)
