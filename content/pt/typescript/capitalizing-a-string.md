---
title:                "Capitalizando uma string"
html_title:           "TypeScript: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por Que

Muitas vezes, precisamos formatar uma string de maneira específica, como deixá-la toda em maiúsculas ou apenas com a primeira letra de cada palavra maiúscula. Nesses casos, capitalizar a string é uma tarefa útil e necessária.

## Como Fazer

Para capitalizar uma string em TypeScript, podemos utilizar o método `toUpperCase()` para transformar todas as letras em maiúsculas. Por exemplo:

```TypeScript
const frase = "o tempo é agora";

console.log(frase.toUpperCase()); // O TEMPO É AGORA
```

Se quisermos apenas a primeira letra de cada palavra em maiúscula, podemos utilizar o método `replace()` combinado com uma expressão regular. Por exemplo:

```TypeScript
const frase = "o tempo é agora";

console.log(frase.replace(/\b\w/g, (letra) => letra.toUpperCase())); // O Tempo É Agora
```

## Mergulho Profundo

Além dos métodos mencionados, também podemos utilizar a função `charAt()` para obter uma letra específica da string e depois usar o método `toUpperCase()` para transformá-la em maiúscula. Isso é útil quando precisamos capitalizar apenas uma parte da string.

```TypeScript
const nome = "joão";

const primeiraLetra = nome.charAt(0).toUpperCase(); // J

console.log(primeiraLetra + nome.slice(1)); // João
```

Outra opção é utilizar a biblioteca externa Lodash, que possui um método `capitalize()` para capitalizar automaticamente a primeira letra de uma string.

```TypeScript
import { capitalize } from "lodash";

console.log(capitalize("tempo é dinheiro")); // Tempo é dinheiro
```

## Veja Também

Aqui estão alguns links úteis com mais informações sobre a manipulação de strings em TypeScript:

- [Documentação oficial do TypeScript sobre strings](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Manipulando strings com JavaScript](https://www.w3schools.com/js/js_string_methods.asp)
- [Guia completo sobre a biblioteca Lodash](https://lodash.com/docs/4.17.15)