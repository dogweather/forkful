---
title:                "TypeScript: Comparando duas datas."
simple_title:         "Comparando duas datas."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em TypeScript?

Comparar duas datas em TypeScript pode ser útil em diversas situações, como verificar se uma data é anterior ou posterior a outra, calcular a diferença de tempo entre elas ou até mesmo validar inputs de datas em formulários. Por isso, é importante entender como fazer essa comparação corretamente.

## Como fazer em TypeScript

Comparar datas em TypeScript pode ser feito de forma simples e eficiente utilizando os métodos nativos da classe `Date`. Por exemplo, se precisarmos verificar se uma data é anterior a outra, podemos utilizar o método `getTime()` para obter o valor numérico da data e então compará-lo:

```TypeScript
const data1 = new Date(2021, 0, 1);
const data2 = new Date(2021, 5, 1);

if(data1.getTime() < data2.getTime()) {
  console.log("A data 1 é anterior à data 2");
}
```
O código acima irá imprimir a mensagem "A data 1 é anterior à data 2". Além disso, podemos utilizar os operadores de comparação diretamente com as datas, como `>`, `<`, `<=` e `>=`.

No entanto, é importante notar que, ao utilizar apenas os métodos `getDate()`, `getMonth()` e `getFullYear()`, podemos ter problemas com datas que possuem horário, pois eles retornam apenas a parte da data e ignoram o horário. Nesses casos, é recomendado utilizar o método `setHours(0,0,0,0)` para zerar o horário antes de fazer a comparação.

## Deep Dive

Se desejarmos fazer uma comparação mais precisa entre datas, podemos utilizar a biblioteca `moment.js` em nosso projeto TypeScript. Com ela, podemos realizar cálculos e manipulações de datas de forma mais avançada, utilizando os métodos `diff()`, `add()`, `subtract()` e muitos outros.

Outro ponto importante a se destacar é que as datas em JavaScript possuem uma precisão de milissegundos, o que pode causar problemas ao fazer comparações diretas com outras linguagens que possuem apenas precisão de segundos. Em situações como essa, é recomendado converter a data em um formato compatível, como o padrão ISO 8601 (`yyyy-mm-ddTHH:mm:ssZ`).

## Veja também

- [Documentação nativa do TypeScript](https://www.typescriptlang.org/docs/handbook/utility-types.html#picktype-keys)
- [Documentação da biblioteca moment.js](https://momentjs.com/docs/#/manipulating/)