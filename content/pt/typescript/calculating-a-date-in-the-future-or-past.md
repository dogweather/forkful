---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "TypeScript: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Calcular uma data no futuro ou passado é uma tarefa comum na programação, onde alteramos uma data base considerando um certo número de dias, meses ou anos. Os programadores fazem isso por diversos motivos, entre eles, para gerenciar eventos, deadlines, ou sequências de tempo em qualquer aplicação que use datas.

## Como fazer:
Aqui estão exemplos de como calcular datas futuras ou passadas em TypeScript:
```TypeScript
let hoje: Date = new Date();  //data atual
let futuro: Date = new Date();
futuro.setDate(hoje.getDate() + 30);  // data 30 dias à frente do hoje

let passado: Date = new Date();
passado.setDate(hoje.getDate() - 30);  // data 30 dias atrás do hoje

console.log("Hoje é: ", hoje);
console.log("Daqui a 30 dias será: ", futuro);
console.log("30 Dias atrás foi: ", passado);
```
Este programa irá imprimir a data de hoje, a data de 30 dias a partir de hoje e a data de 30 dias atrás.

## Mergulho Profundo

1. Histórico: O cálculo de datas tem sido um aspecto fundamental na programação desde seus primórdios, com aplicações que vão desde cronogramas de voo e coordenação de datas de vencimento, a análises de tendências e projeções.

2. Alternativas: Além do método `setDate()`, temos outros métodos como `setFullYear()`, `setMonth()`, `setHours()`, `setMinutes()`, `setSeconds()`, e `setMilliseconds()`. Esses métodos permitem ajustar nos valores de data, hora, minutos, segundos e milissegundos.

3. Implementação: No TypeScript, a manipulação de datas é feita através do objeto `Date`, que representa uma data e hora. O objeto fornece uma série de métodos para realizar operações em datas.

## Veja também:

1. [Guia autodidata de TypeScript para manipulação de datas](https://typescriptlang.org/docs/handbook/utility-types.html)
2. [Documentação oficial do TypeScript - Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
3. [Objeto Date em TypeScript](https://www.typescriptlang.org/docs/handbook/2/objects.html)