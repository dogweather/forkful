---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "TypeScript: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Por que?

Excluir caracteres que correspondem a padrões é uma ação comum em programação. Isso envolve remover caracteres específicos de uma string com base em algum critério definido, como uma expressão regular. Os programadores muitas vezes fazem isso para melhorar a precisão e a eficiência na manipulação de dados.

## Como fazer:

```TypeScript
// Exemplo de exclusão de números de uma string
let string = "Olá, 123 mundo! 456";
let novaString = string.replace(/[0-9]/g, '');
console.log(novaString) // Saída: "Olá, mundo!"
```

```TypeScript
// Exemplo de exclusão de vogais de uma string
let string = "Abacaxi com banana";
let novaString = string.replace(/[aeiou]/ig, '');
console.log(novaString) // Saída: "bcbn"

```

## Mergulho Profundo:

Excluir caracteres correspondentes a um padrão é uma prática antiga na programação, datando desde os dias da linguagem C. Além da utilização de expressões regulares, existem também outras abordagens disponíveis, como o método `split()` e o uso de bibliotecas como o lodash.

Ao excluir caracteres que correspondem a um padrão, é importante considerar a eficiência e o desempenho do código, especialmente em casos de manipulação de grandes quantidades de dados. É recomendado também que o programador entenda bem o padrão utilizado para garantir que a ação seja realizada corretamente.

## Veja também:

- [Documentação TypeScript sobre Expressões Regulares](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Método split() em MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [lodash library](https://lodash.com/)