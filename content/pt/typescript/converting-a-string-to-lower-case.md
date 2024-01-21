---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:39:27.028159-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?

Converter uma string para minúsculas é basicamente transformar todos os caracteres de uma frase ou palavra para o formato de letra pequena. Programadores fazem isso para padronizar entradas de texto, facilitando comparações e validações sem se preocupar com diferenças entre maiúsculas e minúsculas.

## Como Fazer:

No TypeScript, a conversão de uma string para caixa baixa é direta com o uso do método `.toLowerCase()`. Veja abaixo:

```typescript
let frase: string = "Olá, PROGRAMADORES!";
let fraseMinúscula: string = frase.toLowerCase();

console.log(fraseMinúscula); // Saída: "olá, programadores!"
```

Simples, não é?

## Mergulho Profundo

Historicamente, manipular strings é uma necessidade desde os primórdios da programação. Unix e linguagens como C e Java já ofereciam funcionalidades para manipulação de caixa. Hoje em dia, com a globalização e localização de software, lidar com diferentes alfabetos e regras de caixa se tornou mais complexo.

Existem alternativas, como o uso de expressões regulares ou funções nativas de outros idiomas, mas `.toLowerCase()` é a forma mais prática e direta em TypeScript e JavaScript.

A implementação leva em conta o Unicode, o que significa que não se limita apenas ao alfabeto inglês, conseguindo converter caracteres de vários idiomas. Contudo, é importante ter em mente que algumas línguas possuem regras específicas que não são totalmente abrangidas por este método.

## Veja Também

- Documentação da Mozilla sobre `.toLowerCase()`: [MDN toLowerCase](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- Guia de Unicode para Programadores: [Unicode.org](https://home.unicode.org/)
- Artigo sobre Unicode e JavaScript: [JavaScript e Unicode](https://mathiasbynens.be/notes/javascript-unicode)