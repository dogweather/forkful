---
title:                "Extraindo subcadeias"
html_title:           "TypeScript: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?
A extração de substrings é o ato de separar uma parte específica de uma string maior. Programadores tipicamente utilizam essa técnica para manipular e processar dados de forma mais eficiente.

## Como Fazer:
Extrair uma substring em TypeScript é simples e pode ser feito de várias maneiras. Uma opção é utilizar o método `substring()` que recebe como parâmetro o índice inicial e final da substring desejada. Veja o exemplo abaixo:

```Typescript
let stringExemplo = "Hello World";
let substring = stringExemplo.substring(0, 5);
console.log(substring); // irá imprimir "Hello"
```

Outra forma é utilizando o método `slice()`, que é semelhante ao `substring()` mas aceita valores negativos para contar a partir do final da string. Veja o exemplo abaixo:

```Typescript
let stringExemplo = "Hello World";
let substring = stringExemplo.slice(-5);
console.log(substring); // irá imprimir "World"
```

## Profundando:
A extração de substrings pode ser rastreada até as primeiras linguagens de programação, como o C, e é amplamente utilizada em linguagens modernas como o JavaScript e TypeScript. Alternativas para a extração de substrings incluem o uso de expressões regulares e o uso de métodos como `split()` e `substr()`. A implementação da extração de substrings envolve a manipulação de índices e a criação de novas strings a partir da string original.

## Veja Também:
- [Documentação oficial do método substring() do TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#substring)
- [Utilizando expressões regulares em TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Alternativas para a extração de substrings em TypeScript](https://www.geeksforgeeks.org/alternatives-for-string-substring-method-in-typescript/)