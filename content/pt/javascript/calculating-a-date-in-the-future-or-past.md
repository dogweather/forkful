---
title:                "Javascript: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Porque
Calculating dates can be a useful skill when working with time-sensitive data, such as project deadlines or event scheduling. It allows you to quickly find out the date of a specific day in the past or in the future, making it easier to plan and organize your tasks.

## Como Fazer
Para calcular uma data no futuro ou no passado, podemos utilizar a biblioteca nativa Date do Javascript. Vamos ver alguns exemplos práticos para entender melhor como isso funciona.

Primeiro, vamos definir uma data de referência, que será a data que iremos calcular a partir dela. Por exemplo: 

```Javascript
const dataReferencia = new Date();
```

Para calcular uma data no futuro, podemos utilizar o método `setDate()` e passar um valor positivo como parâmetro, que será somado à data de referência.

```Javascript
// Calculando a data de 1 dia no futuro
dataReferencia.setDate(dataReferencia.getDate() + 1);
console.log(dataReferencia); // Output: Sat Aug 29 2020 23:57:34 GMT-0300 (Brasilia Standard Time)

// Calculando a data de 5 dias no futuro
dataReferencia.setDate(dataReferencia.getDate() + 5);
console.log(dataReferencia); // Output: Thu Sep 03 2020 23:57:34 GMT-0300 (Brasilia Standard Time)
```

Da mesma forma, para calcular uma data no passado, podemos utilizar o método `setDate()` e passar um valor negativo como parâmetro, que será subtraído da data de referência.

```Javascript
// Calculando a data de 1 dia no passado
dataReferencia.setDate(dataReferencia.getDate() - 1);
console.log(dataReferencia); // Output: Fri Aug 28 2020 23:57:34 GMT-0300 (Brasilia Standard Time)

// Calculando a data de 5 dias no passado
dataReferencia.setDate(dataReferencia.getDate() - 5);
console.log(dataReferencia); // Output: Sun Aug 23 2020 23:57:34 GMT-0300 (Brasilia Standard Time)
```

## Mergulho Profundo
Além dos métodos `setDate()` mostrados acima, a biblioteca Date também possui outros métodos que podem ser úteis para calcular datas no futuro ou no passado. Alguns exemplos são: `setFullYear()`, `setMonth()` e `setFullYear()`, que permitem alterar o ano, mês e ano completo de uma data respectivamente.

Além disso, é possível realizar cálculos mais complexos utilizando a biblioteca Moment.js, que fornece uma série de métodos para manipular e formatar datas.

## Veja Também
- [Documentação da biblioteca Date do Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Documentação da biblioteca Moment.js](https://momentjs.com/)
- [Artigo: Como trabalhar com datas no Javascript](https://blog.betrybe.com/javascript/datas-javascript/)