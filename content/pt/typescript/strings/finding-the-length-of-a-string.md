---
date: 2024-01-20 17:48:14.619650-07:00
description: "How to: Em TypeScript, a propriedade `length` \xE9 a forma padr\xE3\
  o de obter o comprimento de uma string. Aqui est\xE3o alguns exemplos."
lastmod: '2024-03-13T22:44:46.317004-06:00'
model: gpt-4-1106-preview
summary: "Em TypeScript, a propriedade `length` \xE9 a forma padr\xE3o de obter o\
  \ comprimento de uma string."
title: Descobrindo o comprimento de uma string
weight: 7
---

## How to:
Em TypeScript, a propriedade `length` é a forma padrão de obter o comprimento de uma string. Aqui estão alguns exemplos:

```typescript
let greeting: string = "Olá, mundo!";
console.log(greeting.length);  // Saída: 12

let emptyString: string = "";
console.log(emptyString.length);  // Saída: 0

let stringWithEmoji: string = "Olá 👋";
console.log(stringWithEmoji.length);  // Surpresa! Saída: 6
```

## Deep Dive
Historicamente, obter o comprimento de uma string é uma operação básica nas linguagens de programação e, no TypeScript, segue o mesmo conceito do JavaScript. `length` retorna o número de unidades de código UTF-16 na string, o que significa que emojis ou outros caracteres compostos por múltiplas unidades de código podem aumentar a contagem inesperadamente.

Alternativas à `length` incluem escrever uma função para iterar sobre a string e contar os caracteres, considerando caracteres Unicode corretamente. Ainda assim, na maioria dos casos, `length` satisfaz as necessidades comuns.

Detalhes de implementação para lembrar:
- `length` é uma propriedade de acesso, não uma função. Portanto, não utilize parênteses.
- TypeScript compila para JavaScript, o que significa que `length` vem do protótipo de `String` do próprio JavaScript.

## See Also
- Documentação oficial do TypeScript: [TypeScript Language Specification](https://www.typescriptlang.org/docs/)
- Detalhes sobre UTF-16 e JavaScript: [Understanding JavaScript's UTF-16](https://mathiasbynens.be/notes/javascript-encoding)
- Para práticas avançadas de manipulação de strings: [JavaScript String Methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
