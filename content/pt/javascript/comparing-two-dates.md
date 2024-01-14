---
title:                "Javascript: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Comparar datas é uma tarefa comum em programação, especialmente em aplicações financeiras ou de gerenciamento de eventos. Por exemplo, em um sistema de reservas de hotel, podemos precisar comparar as datas de check-in e check-out para garantir que a reserva esteja dentro do período escolhido pelo cliente.

## Como fazer

Para comparar duas datas em Javascript, podemos usar o objeto Date e seu método "getTime()", que retorna o número de milissegundos desde 1 de janeiro de 1970 até a data especificada. Então, podemos simplesmente subtrair o valor retornado de duas datas diferentes para obter a diferença em milissegundos e fazer as devidas comparações. Veja o exemplo abaixo:

```Javascript
let data1 = new Date('2021-02-10'); // 10 de fevereiro de 2021
let data2 = new Date('2021-02-15'); // 15 de fevereiro de 2021

let diferenca = data2.getTime() - data1.getTime();
console.log(diferenca); // saída: 432000000 (5 dias em milissegundos)
```

Além disso, podemos usar os operadores de comparação, como ">=", "<=" ou "==", para comparar os valores das datas diretamente. No entanto, é importante ter em mente que quando comparamos datas no formato de string, devemos certificar de que elas estejam no formato "AAAA-MM-DD" para garantir uma comparação correta.

## Profundidade

Comparar datas pode parecer uma tarefa simples, mas existem algumas nuances que devemos ter em mente. Por exemplo, ao trabalhar com datas e horários, devemos levar em consideração o fuso horário e o horário de verão. Além disso, o objeto Date em Javascript funciona de forma diferente em navegadores diferentes, o que pode causar resultados inesperados ao comparar datas.

Existem também bibliotecas externas, como o Moment.js, que oferecem métodos mais robustos para comparar datas, levando em conta todas essas nuances e garantindo consistência em diferentes ambientes.

## Veja também

- [Documentação do objeto Date em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Comparações de datas em Javascript](https://www.w3schools.com/js/js_date_methods_compare.asp) (em inglês)