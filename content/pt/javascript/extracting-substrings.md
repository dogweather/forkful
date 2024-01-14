---
title:                "Javascript: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extração de substrings é útil para programadores

Extração de substrings é uma técnica muito útil na programação Javascript, especialmente quando se trabalha com grandes strings de texto. Com essa técnica, é possível obter partes específicas do texto de maneira rápida e eficiente, facilitando a manipulação e análise das informações contidas na string. 

## Como extrair substrings em Javascript

Para extrair substrings em Javascript, utilizamos o método `substring()` que recebe dois parâmetros: o índice inicial e o índice final desejado. Por exemplo: 

```Javascript
let str = "Hello World";
let substring = str.substring(6, 11);
console.log(substring); // Output: World
```

Neste exemplo, extraímos a substring "World" a partir do índice 6 até o índice 10 da string original. É importante notar que o índice final não é incluído na substring resultante.

Além do `substring()`, também é possível utilizar o método `slice()` para extrair substrings em Javascript. A diferença entre eles é que o `substring()` não aceita índices negativos, enquanto o `slice()` permite a contagem a partir do final da string. 

```Javascript
let str = "Hello World";
let substring = str.slice(-5, -1);
console.log(substring); // Output: Worl
```

## Uma visão mais profunda sobre a extração de substrings

Além de extrair substrings a partir de índices específicos, também é possível utilizar outros métodos para encontrar e extrair substrings baseadas em certas condições. Por exemplo, o método `indexOf()` retorna o primeiro índice onde uma determinada substring é encontrada na string original. Podemos então utilizar esse índice como parâmetro para o método `substring()` ou `slice()` para extrair a substring desejada. 

```Javascript
let str = "Hello World";
let index = str.indexOf("W");
let substring = str.substring(index);
console.log(substring); // Output:World
```

Outro método útil é o `split()` que divide uma string em um array de substrings, utilizando um caractere ou expressão regular como separador. 

```Javascript
let str = "Hello World";
let words = str.split(" "); // Separa em palavras
console.log(words); // Output: ["Hello", "World"]
```

## Veja também

- [Documentação MDN sobre o método substring()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Documentação MDN sobre o método slice()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Documentação MDN sobre o método indexOf()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)
- [Documentação MDN sobre o método split()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/split)