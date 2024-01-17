---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Javascript: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que & Por que?

Calcular uma data no futuro ou no passado é uma técnica comumente usada por programadores para lidar com datas em seus projetos. Isso permite que eles criem aplicações dinâmicas que possam trabalhar com diferentes cenários de tempo, como planejar eventos futuros ou rastrear o histórico de eventos passados.

## Como fazer:

```Javascript
// Calculando uma data no futuro em JavaScript
let hoje = new Date(); // Cria um objeto Data contendo a data e hora atuais
let dataFutura = new Date(); // Cria um objeto Data vazio
dataFutura.setDate(hoje.getDate() + 7); // Seta a data para 7 dias a partir da data atual
console.log(dataFutura); // Imprime a data futura no console

// Calculando uma data no passado em JavaScript
let hoje = new Date(); // Cria um objeto Data contendo a data e hora atuais
let dataPassada = new Date(); // Cria um objeto Data vazio
dataPassada.setDate(hoje.getDate() - 7); // Seta a data para 7 dias antes da data atual
console.log(dataPassada); // Imprime a data passada no console
```

Saída:
```
Data futura: Fri Mar 19 2021 22:34:50 GMT-0300 (Horário Padrão de Brasília)
Data passada: Fri Mar 5 2021 22:34:50 GMT-0300 (Horário Padrão de Brasília)
```

## Mergulho Profundo:

Calcular datas no futuro ou no passado é algo que tem sido feito desde o início da programação. No passado, isso era feito com algoritmos mais complexos, mas o JavaScript possui métodos convenientes como `setDate()` e `getDate()` para facilitar esse processo. Existem também bibliotecas e frameworks que oferecem funcionalidades mais avançadas para trabalhar com datas, como o Moment.js. É importante que os programadores entendam como as datas são armazenadas e manipuladas em seus projetos para garantir a precisão e consistência dos dados.

## Veja também:

- [Documentação oficial do JavaScript para Date()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/docs/)
- [Understanding Date and Time in JavaScript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)