---
date: 2024-01-20 17:51:09.475999-07:00
description: "Interpolar strings \xE9 o processo de misturar vari\xE1veis, express\xF5\
  es e texto dentro de uma string. Programadores fazem isso para tornar o c\xF3digo\
  \ mais\u2026"
lastmod: '2024-03-11T00:14:20.683567-06:00'
model: gpt-4-1106-preview
summary: "Interpolar strings \xE9 o processo de misturar vari\xE1veis, express\xF5\
  es e texto dentro de uma string. Programadores fazem isso para tornar o c\xF3digo\
  \ mais\u2026"
title: Interpolando uma string
---

{{< edit_this_page >}}

## O Quê & Porquê?
Interpolar strings é o processo de misturar variáveis, expressões e texto dentro de uma string. Programadores fazem isso para tornar o código mais legível, para evitar a bagunça de concatenar strings e variáveis, e para dinamizar a criação de texto.

## Como Fazer:
```javascript
let nome = 'João';
let saudacao = `Olá, ${nome}! Como você está?`;
console.log(saudacao); // Output: Olá, João! Como você está?

let preco = 19.99;
let produto = 'camiseta';
let mensagem = `O preço da ${produto} é R$${preco.toFixed(2)}.`;
console.log(mensagem); // Output: O preço da camiseta é R$19.99.
```

## Mergulho Profundo
Antigamente no JavaScript, a interpolação de strings era uma dor de cabeça que envolvia uma enxurrada de sinais de mais (`+`) para concatenar variáveis e strings. Com o ES6, lançado em 2015, vieram os template literals, delimitados por crases (`), que simplificaram a interpolação. Além dos template literals, podíamos usar a função `replace()` ou outras bibliotecas como lodash/template para interpolar strings. A implementação atual é nativa e otimizada, proporcionando uma experiência de programação mais agradável e eficiente.

## Veja Também
- MDN Web Docs sobre Template Literals: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Template_literals
- Documentação de lodash/template: https://lodash.com/docs/#template
- Artigo sobre as novidades do ES6: https://medium.com/@raphalima8/es6-em-detalhes-i-5fb60970e6a
