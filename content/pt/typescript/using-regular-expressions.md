---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Expressões regulares são padrões usados para combinar sequências de caracteres em strings. Programadores as utilizam para busca e substituição de texto, validação de dados e manipulação de strings de maneira eficiente e concisa.

## How to:
```TypeScript
const texto: string = "O TypeScript é incrível em 2023!";
const regex: RegExp = /\bTypeScript\b/g;

// Encontrar correspondência
console.log(texto.match(regex)); // Saída: ['TypeScript']

// Substituir texto
console.log(texto.replace(regex, "JavaScript")); // Saída: 'O JavaScript é incrível em 2023!'

// Testar se existe correspondência
console.log(regex.test(texto)); // Saída: true
```

## Deep Dive
Expressões regulares têm origem nos trabalhos teóricos de Stephen Kleene na década de 1950. Enquanto há alternativas, como métodos de string específicos (`indexOf`, `startsWith`, `endsWith`), regex oferece mais flexibilidade. Em TypeScript, se implementa através do objeto `RegExp` do JavaScript e do método `string` correspondente.

## See Also
- [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Regex Tester and Debugger](https://regex101.com/)
