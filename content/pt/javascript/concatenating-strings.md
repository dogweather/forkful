---
title:    "Javascript: Concatenando strings"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que

Em programação, muitas vezes precisamos unir diferentes pedaços de texto em uma única string. Isso é conhecido como concatenação de strings e é uma habilidade fundamental para todo programador de JavaScript.

## Como Fazer

A concatenação de strings pode ser feita de duas maneiras: usando o operador de adição (+) ou o método `concat()`. Vamos dar uma olhada em ambos os métodos com alguns exemplos de código.

O primeiro método, usando o operador de adição, envolve simplesmente colocar os pedaços de texto que queremos unir entre aspas e separá-los com o operador (+). Vamos ver como isso funciona em um código simples:

```Javascript
let saudacao = "Olá";
let nome = "Maria";
let mensagem = saudacao + " " + nome + "! Bem-vinda ao meu blog.";
console.log(mensagem);
```

Neste exemplo, primeiro definimos as variáveis `saudacao` e `nome` como strings e depois as usamos com o operador de adição para criar uma nova string, que é a mensagem de boas-vindas para a Maria.

O segundo método, usando o método `concat()`, é semelhante ao primeiro, mas em vez de usar o operador de adição, usamos o método `concat()` com as strings que queremos unir. Aqui está um exemplo:

```Javascript
let sobrenome = "Silva";
let nomeCompleto = nome.concat(" ", sobrenome);
console.log(nomeCompleto);
```

Neste exemplo, usamos o método `concat()` para adicionar o sobrenome à variável `nome` e, assim, obter o nome completo da Maria.

## Profundando no Assunto

Embora a concatenação de strings possa parecer simples, é importante estar ciente de algumas nuances. Primeiro, o operador de adição (+) não funcionará sempre que quisermos combinar diferentes tipos de dados em uma string. Por exemplo, se tentarmos concatenar uma string com um número, o resultado será uma string contendo ambos em vez da soma dos dois.

Além disso, é importante ter cuidado com a utilização de múltiplos operadores de adição (+) para concatenar várias strings. Isso pode se tornar difícil de acompanhar e pode levar a erros. É sempre recomendável usar o método `concat()` ou o template literals do ES6, que é uma maneira mais fácil e clara de unir strings.

## Veja Também

- [Um guia completo sobre strings em JavaScript](https://www.freecodecamp.org/news/javascript-strings-and-the-string-data-type-explained/)
- [O que são template literals e como usá-los em JavaScript](https://www.digitalocean.com/community/tutorials/js-template-literals)
- [Documentação MDN sobre concat()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)