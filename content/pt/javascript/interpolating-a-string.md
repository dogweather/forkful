---
title:                "Interpolando uma string"
html_title:           "Javascript: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Interpolação de string é um recurso na linguagem de programação Javascript que nos permite combinar texto e variáveis em uma única string. Isso nos permite criar strings dinâmicas, com valores que podem variar dependendo do contexto. Programadores geralmente usam interpolação de string para tornar seu código mais legível e para criar mensagens personalizadas para os usuários.

## Como fazer:

Para interpolar uma string em Javascript, usamos a sintaxe `${variavel}` dentro de uma string delimitada por crases. Por exemplo:

```
const nome = "Maria";
console.log(`Olá ${nome}, bem-vindo ao nosso site!`);
```

Isso imprimirá no console: `Olá Maria, bem-vindo ao nosso site!` onde o valor da variável `nome` é inserido na string. Podemos interpolar múltiplas variáveis em uma única string e também podemos realizar operações matemáticas dentro das chaves `${}`.

## Mergulho profundo:

A interpolação de string é um recurso que foi introduzido na versão ES6 do Javascript. Anteriormente, os programadores usavam a concatenação de strings (usando o operador `+`) para combinar texto e variáveis. No entanto, a interpolação de string é mais legível e permite o uso de expressões dentro das chaves `${}`. É importante notar que nem todos os navegadores suportam a interpolação de string, portanto, é recomendável usar um transpilador como o Babel para garantir a compatibilidade com todos os navegadores.

## Veja também:

- [Documentação sobre interpolação de string em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/template_strings)
- [Tutorial sobre interpolação de string no site Codecademy](https://www.codecademy.com/learn/introduction-to-javascript/modules/interpolation-js)
- [Exemplo de uso de interpolação de string em um projeto real](https://github.com/damiancipolat/Jwt-NodeExpress-Mongodb/blob/master/src/database/mongo_repository.js)