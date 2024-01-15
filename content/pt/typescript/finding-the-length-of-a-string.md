---
title:                "Encontrando o comprimento de uma string"
html_title:           "TypeScript: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Em alguns casos, encontrar o comprimento de uma string pode ser uma tarefa importante em projetos de desenvolvimento. Por exemplo, ao validar uma entrada de usuário ou ao realizar operações matemáticas com base na string.

## Como Fazer

Existem algumas maneiras de encontrar o comprimento de uma string em TypeScript. Aqui estão duas opções:

```TypeScript
// Usando o método length
const string = "Olá, mundo!";
console.log(string.length); // output: 11

// Usando a propriedade length da interface String
const string2: String = "Hello, World!";
console.log(string2.length); // output: 13
```

## Mergulho Profundo

Ao utilizar o método "length", a string é convertida para um array e, em seguida, o comprimento é calculado. Já ao utilizar a propriedade length da interface String, é feito um loop pela string em busca de caracteres, resultando em um pouco mais de processamento.

Também é importante notar que, sendo TypeScript uma linguagem de tipagem estática, é possível utilizar apenas valores do tipo string ao encontrar o comprimento de uma string.

## Ver também

- [Documentação do método length (em inglês)](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [Documentação da interface String (em inglês)](https://www.typescriptlang.org/docs/handbook/variable-declarations.html#string)