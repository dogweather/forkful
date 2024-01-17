---
title:                "Comparando duas datas"
html_title:           "Javascript: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Comparar duas datas é uma tarefa comum para programadores. Isso envolve verificar se uma data é anterior, posterior ou igual a outra data. Os programadores geralmente fazem isso para validação de formulários, cálculos de tempo ou para classificar informações em ordem cronológica.

## Como fazer:

Para comparar duas datas em Javascript, podemos usar o operador de comparação `>`,` <` ou `==`. Por exemplo:

```javascript
let data1 = new Date (2021, 7, 10);
let data2 = new Date (2021, 7, 15);
//Verificar se a data1 é anterior à data2
console.log (data1 < data2); // true
//Verificar se a data1 é posterior à data2
console.log (data1 > data2); // falso
// Verificar se as duas datas são iguais
console.log (data1 == data2); // falso
```

No exemplo acima, criamos duas datas através do construtor `Date` e usamos os operadores de comparação para verificar a relação entre elas. O operador `==` compara as instâncias de data enquanto que `>, <` comparam os valores representados pelas datas.

## Mergulho Profundo:

No passado, antes do Javascript, comparar datas era uma tarefa complexa e exigia muito código para levar em consideração questões como anos bissextos e fuso horário. Com a introdução do objeto `Date` no Javascript, essa tarefa ficou muito mais fácil e eficiente.

Além do operador de comparação, também é possível comparar datas usando os métodos `getTime()` e `getTimezoneOffset()`. O primeiro retorna o tempo em milissegundos a partir de 1º de janeiro de 1970 (conhecido como época Unix) até a data em questão, enquanto o segundo retorna a diferença em minutos entre o fuso horário local e o UTC. Esses métodos podem ser úteis para casos específicos de comparação de datas.

Outras formas de comparar datas em Javascript incluem o uso de bibliotecas externas, como o Moment.js, ou escrever uma função personalizada para lidar com casos mais complexos.

## Veja também:

- [Documentação do objeto Date em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - Biblioteca para manipulação de datas em Javascript](https://momentjs.com/)
- [Comparação de datas em Javascript – Como funciona? [Parte 1]](https://www.tentacode.net/blog/comparacao-de-datas-javascript-parte-1/)