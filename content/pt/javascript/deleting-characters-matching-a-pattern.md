---
title:                "Javascript: Apagando caracteres que correspondem a um padrão"
simple_title:         "Apagando caracteres que correspondem a um padrão"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, ao trabalhar com uma string em Javascript, pode ser necessário remover certos caracteres seguindo um determinado padrão. Isso pode ser útil na validação de formulários, no tratamento de dados ou na manipulação de strings.

## Como Fazer

Para remover caracteres que correspondam a um padrão específico em uma string, podemos usar a função `replace()` em conjunto com expressões regulares. Por exemplo, vamos remover todos os caracteres numéricos de uma string:

```Javascript
let str = "ESte3 é u2m e2xemplo de st3ri4ng co4m núme7ros.";
let newStr = str.replace(/[0-9]/g, "");
console.log(newStr);
```
O output será: "Este é um exemplo de string com números."

Neste exemplo, usamos a expressão regular `[0-9]` que representa qualquer dígito numérico de 0 a 9. Usando a flag `g` ao final da expressão, garantimos que todos os caracteres que correspondam a esse padrão serão removidos da string.

Além disso, podemos usar outros métodos junto com expressões regulares para tornar as manipulações mais precisas. Por exemplo, a expressão `/[\w]/g` corresponde a qualquer caracter alfanumérico. Assim, podemos remover apenas letras maiúsculas de uma string utilizando o método `replace()` em conjunto com a função `toUpperCase()`:

```Javascript
let str = "ESte 3 é u2m e2xemplo de st3ri4ng";
let newStr = str.replace(/[A-Z]/g, "").toUpperCase();
console.log(newStr);
```
O output seria: "  3 é 2 2 3 4".

## Deep Dive

Usar expressões regulares em conjunto com a função `replace()` permite uma manipulação mais precisa de strings. Além disso, é possível utilizar flags para tornar a expressão case-insensitive (ignorando maiúsculas e minúsculas) ou para substituir todas as ocorrências do padrão, mesmo que sejam repetidas na mesma string.

Existem muitas outras possibilidades e combinações de expressões regulares para manipular strings em Javascript. É importante experimentar e praticar para se familiarizar com essa poderosa ferramenta.

## Veja Também

- [MDN web docs: String.prototype.replace()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN web docs: Regular Expressions](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools: JavaScript Regular Expressions](https://www.w3schools.com/js/js_regexp.asp)