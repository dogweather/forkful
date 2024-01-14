---
title:                "Javascript: Convertendo uma data em string"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string é uma tarefa muito comum em programação Javascript. Isso permite que as datas sejam exibidas de forma compreensível para os usuários, em um formato familiar, como "dd/mm/aaaa". Além disso, a conversão de uma data em uma string pode ser útil para armazenar datas em um banco de dados ou ao trabalhar com APIs que requerem datas em formato de string.

## Como converter uma data em uma string?

Para converter uma data em uma string, podemos utilizar o método `toString()` do objeto `Date`. Este método irá retornar uma string contendo a data no formato padrão da sua localidade. Por exemplo:

```Javascript
let data = new Date();
console.log(data.toString()); // Output: Seg Jun 28 2021 12:43:33 GMT-0300 (Horário Padrão de Brasília)
```

Podemos também especificar o formato desejado para a string utilizando os métodos `getDate()`, `getMonth()` e `getFullYear()` para obter o dia, mês e ano respectivamente. Em seguida, podemos concatenar esses valores com as barras "/" para obter o formato de data "dd/mm/aaaa". Por exemplo:

```Javascript
let data = new Date();
let dia = data.getDate();
let mes = data.getMonth() + 1;
let ano = data.getFullYear();
console.log(`${dia}/${mes}/${ano}`); // Output: 28/06/2021
```

## Mergulhando mais fundo

Ao converter uma data em uma string, devemos estar atentos ao fuso horário e localidade do usuário. Isso pode afetar o formato da data e causar confusão nos valores dos dias e meses, pois nem todos os lugares utilizam o mesmo formato. Para evitar problemas, podemos utilizar os métodos `getUTCDate()`, `getUTCMonth()` e `getUTCFullYear()`, que trazem os valores universais da data sem levar em consideração o fuso horário e localidade do usuário.

Além disso, também podemos utilizar bibliotecas de terceiros, como o Moment.js, que facilitam a manipulação de datas em Javascript e oferecem uma ampla variedade de opções para conversão de datas em string.

## Veja também

- [Documentação do método toString() no MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Documentação do Moment.js](https://momentjs.com/)