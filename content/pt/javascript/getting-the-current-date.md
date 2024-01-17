---
title:                "Obtendo a data atual"
html_title:           "Javascript: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é e por quê?
Obter a data atual é uma tarefa comum na programação. Isso permite que os programadores obtenham informações precisas sobre o tempo atual, seja para rastrear eventos, agendar tarefas ou exibir dados em um formato legível para humanos.

## Como fazer:
Obter a data atual no Javascript é simples e podemos utilizar o objeto `Date`. Podemos criar uma nova instância do objeto `Date` e usar seus métodos para obter diferentes informações sobre a data. Por exemplo:

```Javascript
// Criando uma nova instância do objeto Date
let currentDate = new Date();

// Obtendo o dia atual do mês
let day = currentDate.getDate();

// Obtendo o mês atual (começando em 0 para Janeiro)
let month = currentDate.getMonth() + 1;

// Obtendo o ano atual
let year = currentDate.getFullYear();

// Exemplo de saída: Dia 25 do mês 8 de 2021
console.log(`Dia ${day} do mês ${month} de ${year}`);
```

## Mergulho Profundo:
A obtenção da data atual é suportada pelo Javascript desde sua origem, sendo uma das funcionalidades mais básicas da linguagem. Além disso, existem outros métodos que podem ser usados, como `toLocaleDateString()`, que permite obter a data em um formato de idioma específico. Além disso, com a introdução do ES6, também podemos usar a biblioteca `Moment.js` para lidar com datas e horários de forma mais eficiente e com mais recursos.

## Veja também:
- [Documentação MDN: Objeto Date no Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)