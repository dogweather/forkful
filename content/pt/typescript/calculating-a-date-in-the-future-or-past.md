---
title:                "Cálculo de uma data no futuro ou passado"
html_title:           "TypeScript: Cálculo de uma data no futuro ou passado"
simple_title:         "Cálculo de uma data no futuro ou passado"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calculando uma data no futuro ou no passado pode ser útil para diversas situações, como planejar eventos, acompanhar prazos de vencimento ou até mesmo simular cenários.

## Como Fazer

Para calcular uma data no futuro ou no passado usando TypeScript, podemos utilizar a classe nativa `Date`. Primeiramente, devemos criar uma nova instância dessa classe passando a data atual como argumento. Em seguida, podemos utilizar o método `setDate()` para alterar o dia desejado e o método `setMonth()` para alterar o mês. Veja um exemplo abaixo:

```TypeScript
const dataAtual = new Date();
console.log("Data atual:", dataAtual.getDate(), dataAtual.getMonth() + 1);

dataAtual.setDate(dataAtual.getDate() + 5);
dataAtual.setMonth(dataAtual.getMonth() + 1);
console.log("Data no futuro:", dataAtual.getDate(), dataAtual.getMonth() + 1);
```

O código acima primeiro cria uma instância da classe `Date` com a data atual e imprime essa data no console. Em seguida, utiliza os métodos `setDate()` e `setMonth()` para adicionar 5 dias e 1 mês na data atual, respectivamente. Por fim, imprime a nova data no console.

A saída desse código seria:

```
Data atual: 31 8
Data no futuro: 5 10
```

## Deep Dive

A classe `Date` também possui outros métodos úteis para manipulação de datas, como `setFullYear()`, `setHours()`, `setMinutes()` e `setSeconds()`, que permitem alterar o ano, hora, minutos e segundos, respectivamente.

Além disso, a classe também possui métodos para retornar as informações de data, como `getDay()`, que retorna o dia da semana, e `getTime()`, que retorna o tempo em milissegundos desde 1 de janeiro de 1970.

Para mais informações sobre a classe `Date` e seus métodos, você pode consultar a documentação oficial do TypeScript.

## Veja Também

- Documentação oficial do TypeScript sobre a classe Date: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#date-parsing-with--lib-es2015-_-setting-getters-and-setters-with--lib-es2015-dates
- Tutorial sobre manipulação de datas em JavaScript: https://www.w3schools.com/js/js_dates.asp
- Perguntas frequentes sobre datas em TypeScript: https://stackoverflow.com/questions/tagged/typescript+date