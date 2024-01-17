---
title:                "Convertendo uma string para minúsculas"
html_title:           "TypeScript: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que & Por que?

Converter uma string para letras minúsculas em TypeScript é um processo de transformação de uma sequência de caracteres em letras minúsculas. Os programadores fazem isso para padronizar a cadeia de caracteres e torná-la mais legível para processamento e comparação.

## Como fazer:

```typescript
const myString = 'Olá MUNDO!';

console.log(myString.toLowerCase());
// Saída: olá mundo!
```

O código acima usa o método `toLowerCase()` para transformar a string em letras minúsculas. Isso pode ser útil ao processar dados de entrada de usuários, pois torna a comparação de strings mais fácil, já que não há diferença entre maiúsculas e minúsculas.

## Deep Dive:

Embora a conversão de string para minúsculas possa parecer uma tarefa simples, é importante entender sua importância e funcionamento. Historicamente, as linguagens de programação são sensíveis a maiúsculas e minúsculas, o que significa que elas são tratadas como caracteres diferentes. Novamente, a conversão de string para minúsculas permite que os programadores lidem com strings de maneira mais consistente e previsível.

Além disso, existem opções alternativas de métodos para converter strings, como `toUpperCase()` que transforma a string em letras maiúsculas. Também é possível usar expressões regulares para manipular strings, mas isso pode ser mais complexo e exigir mais conhecimento técnico.

A implementação do método `toLowerCase()` em TypeScript garante que apenas strings sejam aceitas como argumento e não altera o valor original da variável. Isso significa que é seguro usar o método em uma string sem se preocupar com alterar o valor original.

## Veja também:

- [Documentação do método `toLowerCase()` em TypeScript](https://www.typescriptlang.org/docs/handbook/utility-types.html#lowercasestring)

- [Exploiting the Lowercase Tack: A History of Case Manipulation in Programming Languages](https://arstechnica.com/information-technology/2012/11/exploiting-the-lowercase-tack-a-history-of-case-manipulation-in-programming-languages/)