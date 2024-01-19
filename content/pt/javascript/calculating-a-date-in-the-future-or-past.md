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

# Manipulação de Datas em JavaScript: Como calcular uma data no futuro ou passado

## Para Que Serve e Por Quê?

Calcular uma data no futuro ou passado consiste em adicionar ou subtrair dias, meses ou anos de uma data específica. Essa operação é comumente utilizada para definir prazos, calcular idades, ou em qualquer situação onde a temporalidade é relevante para o funcionamento de um sistema.

## Como Fazer:

Em JavaScript, podemos manipular datas usando o objeto Date. Veja como adicionar dias a uma data:

```Javascript
var dataAtual = new Date();
// Adicionando 5 dias
var dataFutura = new Date(dataAtual.setDate(dataAtual.getDate() + 5));
console.log(dataFutura);
```

Para subtrair dias, basta usar um número negativo:

```Javascript
var dataAtual = new Date();
// Subtraindo 5 dias
var dataPassada = new Date(dataAtual.setDate(dataAtual.getDate() - 5));
console.log(dataPassada);
```

## Canoa Funda:

Quando o JavaScript foi criado, em 1995, já havia a necessidade de manipulação de datas. Desde então, o objeto Date tem sido parte integrante da linguagem. 

Existem bibliotecas externas, como Moment.js e Day.js, que oferecem mais funcionalidades e facilidades quando comparadas ao objeto Date nativo do JavaScript. Contudo, essas bibliotecas podem ser desnecessárias se todas as operações que você precisa fazer estão cobertas pelo objeto Date.

A implementação do objeto Date em JavaScript considera o fuso horário local. Isso quer dizer que os resultados das operações podem variar de acordo com o fuso horário do ambiente em que o código está sendo executado.

## Veja Também:

Para mais informações, consulte estas fontes:

- Documentação oficial do [Objeto Date no JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Bibliotecas para manipulação de data: [Moment.js](https://momentjs.com/) e [Day.js](https://day.js.org/)
- Artigo no Stack Overflow sobre [como calcular datas no JavaScript](https://stackoverflow.com/questions/563406/add-days-to-javascript-date)