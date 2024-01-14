---
title:                "TypeScript: Comparando duas datas"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em TypeScript?

Comparar datas é uma tarefa comum e importante ao desenvolver programas em TypeScript. Muitas vezes precisamos verificar se uma data é maior, menor ou igual a outra, ou até mesmo saber a diferença entre duas datas. Por isso, é fundamental entender como realizar essa comparação de forma eficiente e precisa.

## Como fazer isso em TypeScript

Comparar duas datas em TypeScript é bastante simples. Primeiro, precisamos criar duas variáveis com os objetos Date que desejamos comparar:

```TypeScript
let dataUm = new Date('2020-01-01');
let dataDois = new Date('2020-12-31');
```

Em seguida, podemos usar os operadores de comparação (<, >, <=, >=) para verificar a relação entre as datas:

```TypeScript
console.log(dataUm < dataDois); // Output: true
console.log(dataUm > dataDois); // Output: false
console.log(dataUm <= dataDois); // Output: true
console.log(dataUm >= dataDois); // Output: false
```

Também podemos usar o método `getTime()` para obter o valor numérico correspondente à data e realizar a comparação diretamente:

```TypeScript
console.log(dataDois.getTime() - dataUm.getTime()); // Output: 31536000000 (diferença em milissegundos)
```

Além disso, o TypeScript também possui a classe `DateTime` da biblioteca `date-fns` que oferece diversas funções e métodos úteis para trabalhar com datas, incluindo a comparação. Por exemplo, podemos usar o método `isBefore()` para verificar se uma data é anterior a outra:

```TypeScript
import { DateTime } from 'date-fns';

let dataUm = new Date('2020-01-01');
let dataDois = new Date('2020-12-31');

console.log(DateTime.isBefore(dataUm, dataDois)); // Output: true
```

## Aprofundando-se na comparação de datas

Ao comparar duas datas em TypeScript, é importante levar em consideração não apenas o valor numérico da data, mas também o fuso horário. Isso pode afetar o resultado da comparação, uma vez que um mesmo momento pode ser representado de forma diferente em fusos horários distintos.

Outro aspecto a ser considerado é que as datas no TypeScript são objetos mutáveis, ou seja, as operações de comparação podem ser influenciadas por alterações realizadas nessas datas ao longo do código. Por isso, é recomendado usar o método `getTime()` para garantir uma comparação precisa.

## Veja também

- [Documentação do Date no TypeScript](https://www.typescriptlang.org/docs/handbook/utilities.html#date) 
- [Documentação do Date no JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Documentação da biblioteca date-fns](https://date-fns.org/)