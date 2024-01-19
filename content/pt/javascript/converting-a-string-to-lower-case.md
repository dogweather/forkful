---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertendo uma string para letras minúsculas em Javascript

## O que & Por quê?
Transformar uma string para minúsculas significa converter todas as letras maiúsculas da string em minúsculas. Programadores fazem isso frequentemente para melhorar a consistência dos dados, facilitar a comparação de strings e melhorar a pesquisa de textos.

## Como fazer:
Uso do método `toLowerCase()` do Javascript para converter uma string em letras minúsculas. Veja este exemplo:
```Javascript
let minhaString = "Olá, Mundo!";
let stringMinuscula = minhaString.toLowerCase();
console.log(stringMinuscula); // saída: "olá, mundo!"
```

## Mergulho profundo
O Javascript é uma linguagem case-sensitive, o que significa que diferencia maiúsculas de minúsculas. O método `toLowerCase()` foi introduzido no ECMAScript 1, a primeira edição de Javascript, em 1997.

Existem alternativas ao `toLowerCase()`, como o `toLocaleLowerCase()`, que considera as regras de localidade para converter as letras (útil para tratar caracteres especiais que podem variar em diferentes idiomas).

Detalhando mais a implementação do `toLowerCase()`, este método não afeta a string original porque as strings em Javascript são imutáveis. Ele retorna uma nova string com todos os caracteres convertidos para minúsculas.

## Veja também
- [Referência do método toLowerCase() na MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Referência do método toLocaleLowerCase() na MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)