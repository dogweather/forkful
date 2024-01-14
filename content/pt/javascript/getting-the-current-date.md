---
title:                "Javascript: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Obter a data atual é uma tarefa comum na programação Javascript. A data é uma informação essencial para diversas aplicações, seja para registro de eventos, cálculos temporais ou exibição de mensagens personalizadas. Além disso, é uma forma de deixar seu código mais dinâmico e atualizado.

## Como fazer?

Para obter a data atual no Javascript, você pode utilizar o objeto nativo `Date` e seu método `getDate()`. Veja o exemplo abaixo:

```Javascript
let dataAtual = new Date(); // cria um objeto Date com a data e hora atual
let dia = dataAtual.getDate(); // obtém o dia atual
let mes = dataAtual.getMonth() + 1; // obtém o mês atual (lembrando que os meses em Javascript iniciam em 0, por isso é somado 1)
let ano = dataAtual.getFullYear(); // obtém o ano atual

console.log(dia + "/" + mes + "/" + ano); // saída: 19/04/2021
```

Além do `getDate()`, existem outros métodos que podem ser utilizados para obter informações específicas da data, como `getMonth()`, `getFullYear()`, `getHours()`, entre outros. Todos esses métodos retornam valores numéricos, mas você pode utilizar condicionais e arrays para transformá-los em formatos de data e hora mais amigáveis para o usuário.

## Aprofundando-se

O objeto `Date` possui vários outros métodos para manipular a data e hora, como `setDate()`, `setHours()` e `setFullYear()`. Também é possível calcular a diferença entre duas datas e até mesmo criar novos objetos Date com datas personalizadas utilizando o método `setDate()`.

Outra dica importante é utilizar bibliotecas externas, como o Moment.js, que facilitam a manipulação e formatação de datas no Javascript. Não tenha medo de pesquisar e experimentar diferentes formas de trabalhar com datas em suas aplicações.

## Veja também

- [Documentação do objeto Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Guia de uso do Moment.js](https://momentjs.com/docs/) 
- [Tutorial de como calcular a diferença entre duas datas no Javascript](https://www.w3resource.com/javascript-exercises/javascript-date-exercise-8.php)