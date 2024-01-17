---
title:                "Extraindo subcadeias"
html_title:           "Javascript: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que é e por quê?

A extração de subcadeias de texto, também conhecida como substrings, é um processo em que se seleciona uma parte específica de uma string maior. Os programadores fazem isso para manipular e obter apenas as informações necessárias de um texto mais extenso.

## Como fazer:

Para extrair uma substring em Javascript, você pode utilizar o método `substr()` ou `substring()`, que aceitam dois parâmetros: o índice inicial e o comprimento da subcadeia.

Por exemplo: 

```Javascript
let texto = "O trabalho nunca termina";
let substring = texto.substr(2, 7); // retorna "trabalh"
let outraSubstring = texto.substring(0, 6); // retorna "O traba"
```

## Profundando:

A extração de substrings é uma técnica muito comum em programação e tem sido usada há anos para manipular strings. No entanto, hoje em dia existem alternativas como expressões regulares ou funções de alto nível, como `slice()` e `substring()`.

Na implementação de `substr()`, o primeiro argumento é o índice inicial e o segundo é a quantidade de caracteres a serem retornados. Já em `substring()` o primeiro argumento é o índice inicial e o segundo é o índice final, que não é incluído na subcadeia.

## Veja também:

- [Documentação de substr() na MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [Documentação de substring() na MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Tutorial de Strings em Javascript na W3Schools](https://www.w3schools.com/js/js_strings.asp)