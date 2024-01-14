---
title:    "Javascript: Utilizando expressões regulares"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em programação?

As expressões regulares são uma poderosa ferramenta para trabalhar com strings em programação. Elas permitem encontrar padrões específicos em textos, facilitando a manipulação e validação de dados. Usar expressões regulares pode economizar tempo e facilitar o desenvolvimento de aplicações.

## Como usar expressões regulares em Javascript

Para utilizar expressões regulares em Javascript, é necessário utilizar o objeto `RegExp`. É possível criar uma expressão regular literalmente, envolvendo-a com barras ("/") e passando-a como parâmetro para o construtor `RegExp`. Por exemplo:

``` Javascript
let regex = /hello/g; // cria uma expressão regular para encontrar a palavra "hello" em uma string
```

Também é possível criar uma expressão regular utilizando o construtor `RegExp`, passando como parâmetro a expressão regular e as flags desejadas entre aspas. Por exemplo:

``` Javascript
let regex = new RegExp("hello", "g"); // cria a mesma expressão regular do exemplo anterior
```

Para usar a expressão regular em uma string, pode-se utilizar o método `test()` para verificar se a expressão é encontrada na string, ou o método `exec()` para retornar informações sobre o primeiro match da expressão na string. Por exemplo:

``` Javascript
let string = "Hello world!";
regex.test(string); // retorna true
regex.exec(string); // retorna ["Hello"]
```

Também é possível utilizar os métodos `replace()` e `match()` para substituir ou extrair partes da string, respectivamente. Por exemplo:

``` Javascript
string.replace(regex, "bye"); // retorna "bye world!"
string.match(regex); // retorna ["Hello"]
```

## Aprofundando nas expressões regulares

As expressões regulares possuem uma sintaxe própria, com diversas metacaracteres e construções que permitem encontrar padrões mais complexos. Além disso, é possível utilizar as flags `i` (case insensitive) e `m` (multiline) para definir como a expressão deve ser interpretada.

Um recurso muito útil nas expressões regulares é o uso de grupos, delimitados por parênteses, que permitem capturar partes específicas da string. Por exemplo:

``` Javascript
let regex = /Name: (.*) Age: (.*)/;
let string = "Name: John Age: 30";
let match = string.match(regex); // retorna ["Name: John Age: 30", "John", "30"]
```

Além disso, existem outras construções que permitem definir quantidades (como `?`, `+` e `*`) e classes de caracteres (como `[]` e `.`) para tornar as expressões mais precisas.

## Veja também

- [Documentação oficial do JavaScript para expressões regulares](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Expressões regulares no YouTube](https://www.youtube.com/watch?v=dQw4w9WgXcQ)
- [Exercícios práticos de expressões regulares em Javascript](https://regexone.com/)