---
title:                "Calculando uma data no futuro ou passado"
html_title:           "TypeScript: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que e Por que?

Calcular uma data no futuro ou no passado é uma técnica comumente usada pelos programadores para obter uma data específica a partir de uma data inicial. Isso é útil em muitas situações, incluindo cálculos de prazos, agendamento de eventos, agendamento de tarefas, etc. 

## Como fazer:

```TypeScript
// Para calcular uma data no futuro, podemos usar o operador de soma (+) com um número de dias. 
// Por exemplo, para obter a data de amanhã, podemos usar o seguinte código:
const dataAtual = new Date();
dataAtual.setDate(dataAtual.getDate() + 1);
console.log(dataAtual);
// Output: Thu Jul 23 2020 11:35:54 GMT-0400 (Eastern Daylight Time)

// Para calcular uma data no passado, podemos usar o operador de subtração (-) com um número de dias.
// Por exemplo, para obter a data de ontem, podemos usar o seguinte código:
const dataAtual = new Date();
dataAtual.setDate(dataAtual.getDate() - 1);
console.log(dataAtual);
// Output: Tue Jul 21 2020 11:35:54 GMT-0400 (Eastern Daylight Time)
```

## Deep Dive:

A técnica de calcular datas no futuro ou no passado é comum em programação e remonta aos primeiros dias da computação. Antes do uso de linguagens de programação de alto nível, os programadores costumavam contar manualmente as datas usando sistemas de calendário. Com o avanço da tecnologia e o surgimento de linguagens de programação mais modernas, essa tarefa foi simplificada, permitindo aos desenvolvedores calcular datas com muita facilidade. Alternativas para calcular datas incluem o uso de bibliotecas de terceiros e APIs de calendário. A implementação específica pode variar dependendo da linguagem de programação utilizada.

## Veja também:

- [Documentação oficial do Date Object em TypeScript](https://www.typescriptlang.org/docs/handbook/functions.html#date)
- [Tutorial de Data e Hora em TypeScript](https://www.tutorialspoint.com/typescript/typescript_date_time.htm)