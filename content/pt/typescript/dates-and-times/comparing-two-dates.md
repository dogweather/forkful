---
date: 2024-01-20 17:34:19.887590-07:00
description: "Comparar duas datas significa verificar a rela\xE7\xE3o temporal entre\
  \ elas: qual vem antes, se s\xE3o iguais ou qual \xE9 posterior. Programadores fazem\
  \ isso para\u2026"
lastmod: '2024-02-25T18:49:43.960118-07:00'
model: gpt-4-1106-preview
summary: "Comparar duas datas significa verificar a rela\xE7\xE3o temporal entre elas:\
  \ qual vem antes, se s\xE3o iguais ou qual \xE9 posterior. Programadores fazem isso\
  \ para\u2026"
title: Comparando duas datas
---

{{< edit_this_page >}}

## O que é & Por quê?

Comparar duas datas significa verificar a relação temporal entre elas: qual vem antes, se são iguais ou qual é posterior. Programadores fazem isso para manipular eventos, validar prazos, agendar tarefas e mais.

## Como fazer:

```TypeScript
const date1: Date = new Date('2023-03-15T00:00:00Z');
const date2: Date = new Date('2023-03-16T00:00:00Z');

// Verificar se date1 é anterior a date2
console.log(date1 < date2); // true

// Verificar se date1 é igual a date2
console.log(date1.getTime() === date2.getTime()); // false

// Verificar se date1 é posterior a date2
console.log(date1 > date2); // false
```

Output:
```
true
false
false
```

## Aprofundamento

Historicamente, a comparação de datas nos primórdios da computação era menos intuitiva devido a sistemas que não representavam datas de forma unificada. Com o JavaScript e TypeScript, simplificou-se muito usando o objeto global `Date`.

Há alternativas, como bibliotecas de terceiros (por exemplo, Moment.js, Date-fns e Day.js), que oferecem mais funções e facilitam o manuseio de datas e fusos horários. No entanto, elas têm suas próprias curvas de aprendizado e sobrecargas.

Na implementação, ao comparar datas em TypeScript, o método `getTime()` é confiável, pois retorna o valor da data em milissegundos desde 1 de janeiro de 1970 UTC, tornando-se um número que é facilmente comparável.

## Veja também

- MDN Web Docs: Documentação sobre o objeto [Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date).
- TypeScript Deep Dive: [Guide](https://basarat.gitbook.io/typescript/) sobre TypeScript, incluindo como trabalhar com datas.
- Date-fns: Biblioteca moderna para manipular [datas](https://date-fns.org/) no JavaScript.
- Day.js: Biblioteca leve similar ao Moment.js. [Day.js](https://day.js.org/) para manipulação de datas.
