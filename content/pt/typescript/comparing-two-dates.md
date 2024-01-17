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

## O que & por quê?

Comparar duas datas é uma tarefa comum na programação, que consiste em verificar se duas datas são iguais ou se uma é anterior ou posterior à outra. Programadores fazem isso para validar dados, realizar operações matemáticas ou lógicas, e criar algoritmos de classificação de dados ou de cálculos de tempo.

## Como fazer:

Comparar duas datas em TypeScript é simples. Primeiro, é preciso criar dois objetos do tipo `Date`, que representarão as datas a serem comparadas. Em seguida, é possível utilizar o operador de comparação `>` (maior que), `<` (menor que), ou `===` (igual) para verificar a relação entre as duas datas.

```Typescript
// criando os objetos Date
let data1 = new Date('2021-01-01');
let data2 = new Date('2020-12-31');

// comparação utilizando operadores
console.log(data1 > data2); // true
console.log(data1 < data2); // false
console.log(data1 === data2); // false
```

## Deep Dive:

Comparar datas é uma tarefa que existe desde os primórdios da computação, quando programas precisavam lidar com cálculos de tempo e datas para executar tarefas. Hoje, além de ser amplamente utilizado em aplicações, também é possível utilizar bibliotecas ou métodos de linguagens de programação que facilitam a comparação de datas, como é o caso do método `isBefore` da biblioteca `moment.js`, que compara se uma data é anterior a outra.

## Veja também:

- Documentação oficial do TypeScript: https://www.typescriptlang.org/docs/
- Tutorial sobre manipulação de datas em TypeScript: https://www.youtube.com/watch?v=3XCnlEDaRg0
- Biblioteca moment.js para manipulação de datas: https://momentjs.com/