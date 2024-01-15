---
title:                "Concatenando strings"
html_title:           "Javascript: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é uma funcionalidade essencial ao escrever códigos em Javascript. Ao mesclar duas ou mais strings, podemos criar textos dinâmicos e personalizados, tornando nossos programas mais interativos e eficientes.

## Como Fazer

```
const firstName = "Maria";
const lastName = "Silva";

console.log(`Bem-vinda, ${firstName} ${lastName}!`);
```
Output: Bem-vinda, Maria Silva!

```
const hobby = "dançar";
const age = 26;

console.log(`Eu amo ${hobby} e tenho ${age} anos de idade!`);
```
Output: Eu amo dançar e tenho 26 anos de idade!

## Deep Dive

Em Javascript, podemos concatenar strings de duas maneiras: usando o operador "+" ou o método "concat()". Ambos funcionam da mesma forma, mas o método "concat()" é preferido por ser mais eficiente em termos de desempenho. Além disso, quando concatenamos uma string com um número usando o operador "+", ele é automaticamente convertido em string.

Outra maneira interessante de concatenar strings é usando o template literals (ou template strings) introduzido no ES6. Com eles, podemos inserir expressões e variáveis dentro de strings usando o operador "${}". Isso torna o código mais legível e fácil de manusear.

```
const name = "João";
const age = 35;

console.log(`Olá, meu nome é ${name} e eu tenho ${age} anos de idade.`)
```
Output: Olá, meu nome é João e eu tenho 35 anos de idade.

## Veja Também

- [MDN Web Docs: Concatenação de Strings](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Text_formatting#concatena%C3%A7%C3%A3o_de_strings)
- [W3Schools: Javascript Strings](https://www.w3schools.com/js/js_strings.asp)
- [Blog da Alura: Concatenação de Strings em Javascript](https://blog.alura.com.br/concatenacao-de-strings-em-javascript/)