---
title:                "Utilizando expressões regulares"
html_title:           "Javascript: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que é e por que usamos Expressões Regulares?
Expressões Regulares (Regex) são sequências de caracteres que definem um padrão de texto a ser correspondido em uma string. Usando Regex, os programadores podem realizar operações de busca, substituição, validação e extração de informações de maneira mais eficaz e precisa. 

## Como fazer:
Você pode criar uma expressão regular em Javascript usando a classe RegExp ou por meio do uso de literais de expressão regular (entre barras ```/expressão/```). Por exemplo, ```/bola/``` irá corresponder a qualquer string que contenha a palavra "bola". Alguns caracteres especiais também são usados na criação de expressões regulares, como "^" para indicar o início de uma string e "$" para indicar o final. Através do método ```match()``` podemos testar se uma string corresponde a uma expressão regular, retornando um array contendo as correspondências encontradas. Por exemplo:

```Javascript
var string = "Eu amo expressões regulares!"
var regex = /amo/
console.log(string.match(regex))
// saída: ["amo"]
```

## Mergulho Profundo:
As expressões regulares têm sua origem na década de 1950, com a criação dos primeiros sistemas para processamento de texto. Hoje, diversas linguagens de programação têm suporte à utilização de expressões regulares, além de existirem ferramentas online para testá-las. Alternativas para o uso de expressões regulares incluem bibliotecas específicas de manipulação e validação de dados, como o Validator.js. Quanto à implementação em Javascript, as expressões regulares são suportadas em todos os navegadores modernos e é importante levar em consideração as diferenças entre maiúsculas e minúsculas na hora de fazer uma correspondência.

## Veja também:
- [Documentação oficial da classe RegExp em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Ferramenta online para testar expressões regulares](https://regexr.com/)
- [Validator.js - biblioteca para validação de dados em Javascript](https://www.npmjs.com/package/validator)