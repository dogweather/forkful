---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Descobrir o comprimento de uma string significa determinar o número de caracteres que ela contém. Programadores fazem isso para manipular dados de texto, como verificar senhas ou limitar a entrada do usuário.

## Como fazer:

Em TypeScript, obtemos o comprimento de uma string usando a propriedade `.length`. Veja como funciona:

```TypeScript
let palavra: string = "Programação";
console.log(palavra.length); // Saída: 12
```

Neste exemplo, a palavra "Programação" tem 12 caracteres, então a saída é `12`.

## Aprofundamento:

(1) Contexto histórico: A propriedade `.length` tem sido fundamental em linguagens de programação baseadas em JavaScript, como TypeScript, para manipular strings desde o advento da computação moderna.

(2) Alternativas: Embora `.length` seja a maneira padrão e mais eficiente de encontrar o comprimento de uma string em TypeScript, técnicas alternativas podem ser usadas para tarefas específicas. Por exemplo, você pode converter a string em um array com o método `.split("")` e, em seguida, contar os elementos do array.

```TypeScript
let palavra: string = "Programação";
let arrayDaPalavra = palavra.split("");
console.log(arrayDaPalavra.length); // Saída: 12
```
 
(3) Detalhes de Implementação: A propriedade `.length` retorna o número de unidades de código UTF-16 na string. Caracteres Unicode que se enquadram fora do plano multilíngue básico são contados como duas unidades.

## Veja também:

Para saber mais sobre como trabalhar com strings em TypeScript, dê uma olhada nos seguintes recursos:

1. [Manual oficial do TypeScript](https://www.typescriptlang.org/docs/handbook/2/strings.html)
2. [Mozilla Developer Network's JavaScript reference](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/length)
3. [W3Schools' TypeScript Strings](https://www.w3schools.com/typescript/ts_strings.asp)