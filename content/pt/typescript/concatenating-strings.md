---
title:                "Concatenando strings"
html_title:           "TypeScript: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que é e porquê?

Concatenar strings é simplesmente juntar duas ou mais strings em uma única string. Os programadores fazem isso para criar mensagens, nomes de arquivos dinâmicas, formatar dados para visualização, entre outros.

## Como fazer:

```TypeScript
// Exemplo 1:
let str1 = "Olá";
let str2 = "mundo";
let str3 = str1 + " " + str2;
console.log(str3); // Output: "Olá mundo"

// Exemplo 2:
let str4 = "1";
let str5 = "2";
let str6 = str4 + str5;
console.log(str6); // Output: "12"

// Exemplo 3:
let str7 = "O valor é: ";
let num = 28;
let str8 = str7 + num;
console.log(str8); // Output: "O valor é: 28"
```

## Mergulho profundo:

Concatenar strings pode ser considerado uma função básica de programação, já que é usada em muitos projetos. A concatenação pode ser feita com o operador `+` ou com o método `.concat()`. Além disso, também é possível utilizar interpolação de string para adicionar variáveis diretamente em uma string.

## Veja também:

- [Documentação TypeScript para manipulação de strings](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Guia completo sobre concatenação de strings](https://www.javascripttutorial.net/javascript-string-concatenation/)