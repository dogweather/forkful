---
title:                "Analisando uma data a partir de uma string."
html_title:           "TypeScript: Analisando uma data a partir de uma string."
simple_title:         "Analisando uma data a partir de uma string."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e porquê?

Analisar uma data a partir de uma string é o processo de converter uma data representada em formato de texto em um tipo de dado de data. Este é um recurso importante em muitas aplicações, pois permite que os programadores manipulem e gerenciem datas de uma forma mais eficiente e precisa.

## Como fazer:

```TypeScript 
let dateString = '10/31/2021';
let date = new Date(dateString);
console.log(date.toString());
```

Este código irá criar uma nova instância de `Date` usando a string fornecida e, em seguida, imprimir a data formatada para o console.

## Aprofundando:

Analisar datas a partir de strings é uma funcionalidade comumente usada em muitas linguagens de programação. Em TypeScript, podemos usar o construtor `Date` e o método `toString()` para converter uma string em um objeto de data.

Alternativamente, podemos usar bibliotecas de terceiros, como o Moment.js, que oferecem recursos mais avançados e flexíveis para analisar e manipular datas.

Ao analisar datas a partir de strings, também é importante considerar a localização e o formato da data. Por exemplo, em alguns países, o formato de data padrão é DD/MM/AAAA, enquanto em outros é MM/DD/AAAA.

## Veja também:

- [Documentação do TypeScript sobre objetos de data](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [Moment.js documentação](https://momentjs.com/docs/#/parsing/string/)