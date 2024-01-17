---
title:                "Convertendo uma data em uma string"
html_title:           "TypeScript: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?
Converter uma data em uma string é um processo comum entre os programadores, que permite representar uma data de forma legível para humanos. Isso é útil para exibir datas em formato de texto em interfaces do usuário, gerar logs ou salvar dados em um formato específico.

## Como fazer:
```TypeScript 
const myDate = new Date();
const dateString = myDate.toDateString();
console.log(dateString); // Saída: "Thu Jul 22 2021"
```

```TypeScript
const myDate = new Date();
const options: Intl.DateTimeFormatOptions = { weekday: "long", year: "numeric", month: "long", day: "numeric" };
const dateString = myDate.toLocaleDateString("pt-BR", options);
console.log(dateString); // Saída: "quinta-feira, 22 de julho de 2021"
```

## Mergulho profundo:
A necessidade de converter datas em strings surgiu junto com a criação das primeiras linguagens de programação, que não tinham tipos de dados específicos para representar datas. Atualmente, existem diversas formas de realizar essa conversão, como usando bibliotecas externas ou formatando manualmente a string. A implementação nativa em TypeScript utiliza a classe `Date` e os métodos `toDateString()` e `toLocaleDateString()` para realizar a conversão de acordo com a localização do usuário.

## Veja também:
- [Documentação oficial do método toDateString()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
- [Documentação oficial do método toLocaleDateString()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Tudo sobre datas em TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-7.html#support-for-similar-tojson-methods-in-class-declarations)