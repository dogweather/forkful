---
title:                "TypeScript: Convertendo uma data em uma string"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Muitas vezes, ao criar um programa ou aplicativo, é necessário exibir datas para o usuário de uma forma compreensível. Converter uma data em uma string é uma maneira de apresentar essa informação de uma forma clara e legível.

## Como fazer:

```TypeScript 
const date = new Date(); //cria um objeto com a data atual
const stringDate = date.toString(); //converte a data em uma string
console.log(stringDate); //saída: Ter Nov 23 2021 12:00:00 GMT-0500 (Eastern Standard Time)
```

Neste exemplo, usamos o método `toString()` do objeto `Date` para converter a data em uma string. Este método retorna a data em um formato padrão, mas também é possível passar um argumento para personalizar o formato da string.

```TypeScript
const date = new Date();
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
const stringDate = date.toLocaleDateString('pt-BR', options);
console.log(stringDate); //saída: terça-feira, 23 de novembro de 2021
```

Ao utilizar o método `toLocaleDateString()`, podemos especificar o local e opções para mostrar a data de acordo com a preferência do usuário.

## Mergulho Profundo:

Ao converter uma data em uma string, é importante entender como os diferentes métodos trabalham para escolher o mais adequado para a sua necessidade. Alguns métodos comuns para converter uma data em uma string são `toString()`, `toDateString()`, `toTimeString()` e `toLocaleDateString()`. Além disso, existem algumas bibliotecas de terceiros que oferecem mais opções para personalizar o formato da string, como o Moment.js.

É importante também considerar como a data será utilizada em outros países ou regiões, pois o formato de data pode variar. Além disso, é preciso ter cuidado ao manipular strings que contêm datas, já que erros de conversão e interpretação podem ocorrer.

## Veja Também: 

- [Documentação do método toString() no MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Documentação do método toLocaleDateString() no MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Página oficial do Moment.js](https://momentjs.com/)