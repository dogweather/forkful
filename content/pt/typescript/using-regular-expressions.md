---
title:                "TypeScript: Utilizando expressões regulares"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em TypeScript?

Expressões regulares são um recurso poderoso na programação, que permitem selecionar e manipular padrões de texto de forma fácil e rápida. Em TypeScript, elas podem ser usadas para validar entradas de usuário, formatar dados ou filtrar conteúdo de strings. Aprender a usar expressões regulares pode ajudar a tornar o seu código mais eficiente e produtivo.

## Como usar expressões regulares em TypeScript

Para usar expressões regulares em TypeScript, primeiro é necessário criar um objeto de expressão regular usando o operador `new RegExp()`. Dentro dos parênteses, é possível colocar uma sequência de caracteres para criar um padrão de pesquisa. Por exemplo:

```TypeScript
let regex = new RegExp("hello");
```

Isso criará um objeto de expressão regular que procurará pela palavra "hello" em uma string. Para testar essa expressão, podemos usar o método `test()` que retornará `true` se o padrão for encontrado ou `false` caso contrário.

```TypeScript
console.log(regex.test("Hello world")); // Saída: false
console.log(regex.test("Say hello to the world")); // Saída: true
```

Podemos também utilizar expressões regulares incorporadas em métodos de strings, como `match()` e `replace()`, para encontrar e substituir padrões em uma string. Por exemplo:

```TypeScript
console.log("Hello world".match(regex)); // Saída: null (padrão não encontrado)
console.log("Say hello to the world".match(regex)); // Saída: ["hello"]
console.log("Hello world".replace(regex, "hi")); // Saída: Hi world
```

## Aprofundando em expressões regulares em TypeScript

Para criar padrões mais complexos, é possível utilizar metacaracteres em expressões regulares. Por exemplo, o metacaractere `.` representa qualquer caractere único, o `+` indica uma ocorrência ou mais do caractere anterior e o `*` indica 0 ou mais ocorrências. É possível combinar vários metacaracteres para criar padrões mais específicos. Por exemplo:

```TypeScript
let regex = new RegExp("he.+o");

console.log("Hello world".test(regex)); // Saída: true
console.log("Hey there!".test(regex)); // Saída: false
```

Além disso, é possível usar expressões regulares com modificadores, como `i` para fazer a busca ignorar maiúsculas e minúsculas, e `g` para buscar globalmente. Por exemplo:

```TypeScript
let regex = new RegExp("hello", "i");

console.log("Hello world".match(regex)); // Saída: ["Hello"]
```

Existem também muitos outros recursos e metacaracteres que podem ser explorados em expressões regulares. É recomendável consultar documentações e tutoriais para se aprofundar mais nesse assunto.

## Veja também

- [Documentação oficial do TypeScript sobre expressões regulares](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Tutorial sobre expressões regulares em TypeScript](https://javascript.info/regular-expressions)
- [Guia básico de expressões regulares em TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-typescript)