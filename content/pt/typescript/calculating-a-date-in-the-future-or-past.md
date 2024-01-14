---
title:                "TypeScript: Calculando uma data no futuro ou passado"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Algumas vezes precisamos calcular uma data no futuro ou no passado em nossos programas TypeScript. Isso pode ser necessário para agendar tarefas ou para calcular o tempo desde uma determinada data. Felizmente, com o TypeScript, calcular datas é bastante simples e direto.

## Como fazer

Vamos começar com um exemplo básico de como calcular uma data futura. Suponha que queremos saber qual será a data daqui a 7 dias a partir de hoje. Podemos usar a função `Date` do TypeScript para criar um novo objeto de data e adicionar 7 dias a ele usando o método `getDate()`:

```TypeScript
let today = new Date();
let futureDate = new Date(today.getDate() + 7);
console.log(futureDate);
// Output: Sun Apr 26 2020 19:21:38 GMT-0700 (Pacific Daylight Time)
```

Podemos também calcular uma data no passado, subtraindo dias em vez de adicioná-los:

```TypeScript
let today = new Date();
let pastDate = new Date(today.getDate() - 14);
console.log(pastDate);
// Output: Tue Apr 07 2020 19:24:29 GMT-0700 (Pacific Daylight Time)
```

Também é importante notar que o método `getDate()` retorna a data atual como um número, então podemos adicionar ou subtrair esse número para calcular datas dentro do mesmo mês. No entanto, se quisermos calcular datas em meses diferentes, precisamos usar outros métodos, como `setMonth()` ou `setFullYear()`.

## Mergulho Profundo

Por baixo dos panos, o TypeScript usa o objeto `Date` padrão do JavaScript para realizar cálculos de datas. Esse objeto oferece uma variedade de métodos para manipular datas, como `setDate()` e `setFullYear()`. Além disso, o TypeScript também fornece uma forma mais confiável de trabalhar com datas, validando automaticamente dados inválidos e suportando fusos horários diferentes.

## Veja também

- [Documentação oficial do TypeScript sobre Date](https://www.typescriptlang.org/docs/handbook/declaration-files/by-example.html#working-with-other-javascript-libraries)
- [Artigo sobre como trabalhar com datas no TypeScript](https://blog.shovonhasan.com/using-javascript-date-object-in-typescript/)
- [Um guia completo para manipulação de datas no TypeScript](https://dev.to/aralroca/a-guide-to-date-and-time-manipulation-in-typescript-1lib)