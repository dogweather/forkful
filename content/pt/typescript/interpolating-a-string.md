---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# String Interpolation em TypeScript 

## O que & Por quê?

Interpolação de strings é um método para substituir variáveis dentro de strings. Por quê? Para moldar cadeias de caracteres de maneira mais limpa e flexível. 

## Como Fazer:

Aqui estão alguns exemplos de como fazer interpolação de string no TypeScript:

```TypeScript
let nome = "Ricardo";
console.log(`Olá, ${nome}!`);   // Olá, Ricardo!

let x = 10;
let y = 20;
console.log(`O resultado de x + y é ${x + y}`);   // O resultado de x + y é 30
```

Nesses exemplos, usamos a sintaxe `${...}` dentro de strings de template para interpolar variáveis.

## Aprofundamento

A interpolação de string foi introduzida no ECMAScript 6 (também conhecido como ES2015) e é uma adição bem-vinda ao JavaScript e, por extensão, ao TypeScript.

Alternativas para interpolação de string incluem o uso de string concatenation (usando o `+` operador) ou a função `concat()`. Bem:

```TypeScript
let nome = "Ricardo";
console.log("Olá, " + nome + "!"); 

let x = 10;
let y = 20;
console.log("O resultado de x + y é " + (x + y));
```

Embora essas alternativas possam ser usadas, a interpolação de strings tende a ser mais fácil de ler, especialmente quando se trabalha com múltiplas variáveis.

## Veja Também

Para um entendimento mais profundo da interpolação de strings em TypeScript, consulte os links abaixo:

- Documentação oficial do TypeScript sobre [Template Strings](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- Artigo do Mozilla sobre [Literal de modelo](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Template_literals)