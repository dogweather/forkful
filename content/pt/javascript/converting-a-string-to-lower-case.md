---
title:                "Convertendo uma string para minúsculas"
html_title:           "Javascript: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Você já precisou converter uma string para letras minúsculas em seu código Javascript? Isso pode ser útil para padronizar os dados de entrada ou para facilitar comparações de strings. Neste artigo, vamos explorar como realizar essa conversão de forma simples e eficiente.

## Como fazer

Para converter uma string para letras minúsculas em Javascript, podemos utilizar o método `toLowerCase()`. Veja um exemplo abaixo:

```Javascript
let string = "EXEMPLO DE STRING";
console.log(string.toLowerCase());
```

O código acima irá imprimir "exemplo de string" no console. Isso mostra como o método `toLowerCase()` transforma todas as letras maiúsculas em minúsculas.

Um ponto importante a ser destacado é que a conversão para letras minúsculas é realizada de acordo com os padrões da linguagem que o navegador está utilizando. Por exemplo, no idioma turco, algumas letras maiúsculas possuem uma versão diferente em minúsculas. O método `toLowerCase()` leva isso em consideração e realiza a conversão corretamente.

## Aprofundando

Além do método `toLowerCase()`, podemos utilizar outra técnica para converter strings para letras minúsculas em Javascript. Podemos utilizar o operador de atribuição `=` e o método `String.prototype.toLowerCase` em conjunto, como no exemplo abaixo:

```Javascript
let string = "EXEMPLO DE STRING";
string = string.toLowerCase();
console.log(string);
```

A saída será a mesma do exemplo anterior. No entanto, esse método tem a vantagem de ser ligeiramente mais rápido em termos de desempenho.

Além disso, vale lembrar que tanto o método `toLowerCase()` quanto o método `String.prototype.toLowerCase` não alteram a string original, mas sim retornam uma nova string convertida. Portanto, é importante atribuir o resultado a uma variável ou usá-lo diretamente em uma função ou comparação.

## Veja também

Aqui estão alguns recursos adicionais que podem ser úteis para você:

- Documentação oficial do método `toLowerCase()`: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Artigo no site Codecademy sobre conversão de strings em Javascript: https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-strings/cheatsheet