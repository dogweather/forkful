---
date: 2024-01-20 17:51:49.435927-07:00
description: "Como Fazer: Historicamente, a interpola\xE7\xE3o de strings nem sempre\
  \ foi t\xE3o direta em JavaScript. Antes do ECMAScript 2015 (ES6), concatenar strings\
  \ e\u2026"
lastmod: '2024-04-05T22:50:59.574331-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, a interpola\xE7\xE3o de strings nem sempre foi t\xE3o direta\
  \ em JavaScript."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
```TypeScript
let usuario = "João";
let pontos = 200;
// Interpolação de string usando template literals
let mensagem = `Parabéns, ${usuario}! Você ganhou ${pontos} pontos!`;
console.log(mensagem);
```

Saída do exemplo:
```
Parabéns, João! Você ganhou 200 pontos!
```

## Aprofundando
Historicamente, a interpolação de strings nem sempre foi tão direta em JavaScript. Antes do ECMAScript 2015 (ES6), concatenar strings e variáveis era feito através do operador `+`, o que poderia ficar confuso e difícil de ler com strings mais complexas. Com o advento dos template literals em ES6, a interpolação de strings se tornou uma tarefa mais intuitiva e menos propensa a erros, pois permite a inserção de expressões dentro de chaves `${}` em strings delimitadas por crases.

Além dos template literals, existem bibliotecas e métodos alternativos para interpolar strings, como a função `replace` e até mesmo bibliotecas externas que oferecem funcionalidades adicionais. No entanto, para muitos casos de uso, os template literals fornecem uma solução nativa e eficiente.

No que diz respeito a detalhes de implementação, quando você usa a interpolação de strings com template literals, o TypeScript é esperto o suficiente para manter as verificações de tipo. Isso significa que, se você tentar interpolar um valor que não corresponda ao tipo esperado dentro de uma string, o TypeScript alertará sobre o erro potencial.
