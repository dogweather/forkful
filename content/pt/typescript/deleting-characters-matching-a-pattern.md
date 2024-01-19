---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Deletar caracteres que correspondem a um padrão é uma tática de manipulação de string em que removemos todas as ocorrências de um padrão específico em uma string. Programadores fazem isso para limpar ou formatar dados, facilitando a análise e a usabilidade.

## Como Fazemos:

No TypeScript, podemos usar o método replace com uma expressão regular para deletar caracteres que correspondem a um padrão. Aqui está um exemplo:

```TypeScript
let str = "abcdabcd";
let pattern = /a/g; //encontrará todas as ocorrências do caractere 'a'
str = str.replace(pattern, '');
console.log(str); //output: "bcdbcd"
```

No exemplo acima, removemos todas as ocorrências do caractere 'a' na string.

## Aprofundando:

Historicamente, a remoção de caracteres por padrão tem sido um recurso crucial nas linguagens de programação, permitindo aos programadores lidar eficazmente com a manipulação de strings.

Entre as alternativas, temos o laço de loop (como for ou while), embora para grandes conjuntos de dados, o método `replace` com expressões regulares é mais eficiente. No TypeScript, a implementação interna do `replace` usa uma das muitas técnicas de processamento de sequência eficiente, fornecendo um desempenho superior.

## Veja Também:

- [MDN String.prototype.replace()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools Regular Expressions](https://www.w3schools.com/js/js_regexp.asp)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)