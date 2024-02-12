---
title:                "Interpolando uma string"
aliases:
- /pt/typescript/interpolating-a-string.md
date:                  2024-01-20T17:51:49.435927-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Interpolar uma string é o processo de inserir valores de variáveis dentro de uma string literal. Programadores fazem isso para construir strings de maneira dinâmica e legível, facilitando tanto a manipulação de dados quanto a interação com o usuário.

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
