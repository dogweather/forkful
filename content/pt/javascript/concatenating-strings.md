---
title:                "Concatenação de strings"
html_title:           "Javascript: Concatenação de strings"
simple_title:         "Concatenação de strings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que é e por que fazemos isto?

Concatenar strings é combinar duas ou mais strings em uma única string. Isso é útil para unir diferentes partes de uma informação, como palavras, números ou símbolos, em uma única unidade. Na programação, fazemos isso quando queremos criar uma saída de texto personalizada ou para facilitar a manipulação de dados.

## Como fazer:

````javascript
// Usando o operador "+" para concatenar strings:
var saudacao = "Oi, ";
var nome = "Maria";
var mensagem = saudacao + nome; // resultado: Oi, Maria

// Usando o método concat():
var sobrenome = "Silva";
var nomeCompleto = nome.concat(" ", sobrenome); // resultado: Maria Silva
````

## Exploração aprofundada:

A concatenação de strings tem sido usada desde os primeiros dias da programação para criar mensagens personalizadas ou para manipulação de dados. Além do operador "+" e do método concat(), também é possível utilizar o método join() para unir uma série de strings em uma única string com separadores personalizados. Outra opção é usar literais de template, que permitem inserir variáveis diretamente em uma string sem a necessidade de concatenação.

## Ver também:

- [Documentação sobre concatenação de strings em JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Artigo sobre literais de template em JavaScript](https://www.freecodecamp.org/news/an-intro-to-es6-template-literals-a-new-way-to-work-with-strings-in-javascript-675678ae44b4/)