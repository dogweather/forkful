---
title:                "Obtendo a data atual"
html_title:           "TypeScript: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é e por que fazer isso?

Obter a data atual é uma função importante em programação, pois permite que os desenvolvedores obtenham informações precisas sobre o tempo e a data atual. Isso é útil para muitas aplicações, como sistemas de reservas de viagem, aplicativos de calendário e sistemas de gerenciamento de tarefas.

## Como fazer:

Em TypeScript, obter a data atual é fácil com o objeto Date incluído no JavaScript. Para isso, basta usar o construtor Date() sem nenhum argumento. Veja o exemplo abaixo:

```
const dataAtual = new Date();
console.log(dataAtual);

```

A saída será semelhante a esta: ```Thu Jul 15 2021 18:52:46 GMT-0400 (Eastern Daylight Time)```

Se você quiser exibir a data atual em um formato específico, pode usar os métodos disponíveis no objeto Date. Por exemplo, se você quiser exibir apenas o dia, mês e ano, pode usar os seguintes métodos:

```
const dataAtual = new Date();
const dia = dataAtual.getDate();
const mes = dataAtual.getMonth() + 1;
const ano = dataAtual.getFullYear();

console.log(`${dia}/${mes}/${ano}`);
```

Isso irá imprimir a data atual no formato: ```15/07/2021```

## Deep Dive:

Ao longo da história, várias técnicas e métodos foram usados para obter a data atual em programação. Anteriormente, os programadores precisavam lidar com diferentes formatos e fusos horários, o que tornava essa tarefa mais complicada. Com o JavaScript e TypeScript, essa tarefa foi simplificada com o objeto Date.

Uma alternativa ao uso do objeto Date é o uso de bibliotecas de terceiros, como o Moment.js, que facilitam a formatação e manipulação de datas. No entanto, para projetos menores, o uso do objeto Date é geralmente suficiente e evita a necessidade de adicionar dependências externas.

Outro detalhe importante é que o objeto Date do JavaScript é baseado em tempo universal coordenado (UTC), que difere dos fusos horários locais. Por isso, é importante considerar e ajustar o horário de acordo com o fuso horário desejado.

## Veja também:

- Documentação oficial do objeto Date do JavaScript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/