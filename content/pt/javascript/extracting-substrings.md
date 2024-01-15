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

## Por que extrair substrings é útil?

Extrair substrings é útil quando você precisa obter uma parte específica de uma string maior. Isso pode ser útil em situações como manipulação de dados e formatação de texto.

## Como Fazer

Extrair substrings é simples com o uso do método `substring()` em Javascript. Veja abaixo um exemplo de como usar esse método:

```Javascript
let texto = "Olá, mundo!";
let extrair = texto.substring(0, 4);

console.log(extrair);
```

Este código irá retornar "Olá" como saída, pois estamos extraindo os caracteres da posição 0 até a posição 3 da string original.

Você também pode usar índices negativos para extrair uma substring a partir do final da string. Por exemplo:

```Javascript
let texto = "Olá, mundo!";
let extrair = texto.substring(-6);

console.log(extrair);
```

Neste caso, a substring retornada será "mundo!", contando 6 caracteres a partir do final da string original.

## Mergulho Profundo

Além do método `substring()`, existem outras maneiras de extrair substrings em Javascript. Por exemplo, você pode usar o método `slice()` ou realizar manipulações com a propriedade `length` de uma string.

É importante notar que o índice de início do método `substring()` é inclusivo, ou seja, o caracter na posição de início será incluído na substring. Já o índice de fim é exclusivo, ou seja, o caracter na posição de fim não será incluído na substring resultante.

## Veja Também

- [Documentação do método `substring()` em MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Diferenças entre `substring()` e `slice()` em Javascript](https://www.w3schools.com/jsref/jsref_substring.asp)
- [Manipulação de strings em Javascript](https://www.geeksforgeeks.org/javascript-string-manipulation/)