---
title:                "Comparando duas datas"
html_title:           "TypeScript: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Comparar datas é uma tarefa comum ao trabalhar com dados relacionados ao tempo em projetos de desenvolvimento. Ao comparar duas datas, é possível determinar qual delas é mais recente, se são iguais ou se uma é anterior à outra. Isso pode ser útil em diversas situações, como ao validar dados em um formulário, filtrar dados em uma aplicação ou ordenar eventos em uma agenda.

## Como fazer em TypeScript

Comparando duas datas em TypeScript é bastante simples. Primeiro, é preciso ter as duas datas que serão comparadas. Isso pode ser feito de diversas maneiras, como utilizando o construtor de `Date` ou convertendo strings em datas utilizando `Date.parse()`. Depois, basta utilizar os operadores de comparação (`>, <, >=, <=`) para comparar as datas e obter um valor booleano como resultado.

```TypeScript
let data1 = new Date(2021, 8, 1);
let data2 = new Date(2021, 8, 10);

console.log(data1 < data2); // true
console.log(data1 > data2); // false
console.log(data1 === data2); // false
```

Também é possível comparar diretamente as strings que representam as datas utilizando os mesmos operadores de comparação.

```TypeScript
let data1 = "2021-08-01";
let data2 = "2021-08-10";

console.log(data1 < data2); // true
console.log(data1 > data2); // false
console.log(data1 === data2); // false
```

Como pode ser visto nos exemplos acima, as datas são comparadas com base em sua posição no calendário, ou seja, datas mais recentes são consideradas maiores que datas anteriores. Além disso, as datas também levam em conta o horário, então se for necessário ignorar o horário na comparação, é importante definir a hora como zero para ambas as datas. 

## Mergulho Profundo

Por padrão, as datas em JavaScript e TypeScript utilizam o fuso horário do local em que o código está sendo executado. Isso pode causar problemas ao comparar datas em diferentes fusos horários, pois uma data pode ser considerada maior que outra por causa do horário, mesmo que representem o mesmo momento.

Para evitar essa situação, é recomendado utilizar a função `getTime()` para obter o valor numérico de uma data e comparar esses valores em vez das próprias datas. Esse valor representa a quantidade de milissegundos desde a meia-noite de 1º de janeiro de 1970, conhecido como *época*. Ao comparar esses valores, é possível obter resultados mais precisos, independentemente do fuso horário.

```TypeScript
let data1 = new Date(2021, 8, 1, 12, 0, 0); // 12h no fuso horário local
let data2 = new Date(2021, 8, 1, 10, 30, 0); // 10h30 no fuso horário local

console.log(data1.getTime() === data2.getTime()); // false
```

Além disso, também é possível utilizar a biblioteca [Moment.js](https://momentjs.com/), que oferece uma série de funções para manipulação e comparação de datas, facilitando ainda mais essa tarefa.

## Veja também

- [Documentação oficial do TypeScript sobre a classe Date](https://www.typescriptlang.org/docs/handbook/utility-types.html#date)
- [Artigo sobre a biblioteca Moment.js](https://www.freecodecamp.org/news/how-to-compare-two-dates-in-javascript/)
- [Exemplos de uso da função getTime()](https://www.geeksforgeeks.org/javascript-difference-between-two-dates-ms-and-gettime/)