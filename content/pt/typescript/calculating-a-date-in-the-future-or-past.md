---
title:                "TypeScript: Calculando uma data no futuro ou passado."
simple_title:         "Calculando uma data no futuro ou passado."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por quê

Calcular uma data no futuro ou no passado pode ser uma tarefa útil em muitas aplicações, como por exemplo em um calendário ou em uma reserva de viagem. É importante entender como fazê-lo para que você possa escrever códigos eficientes e precisos.

## Como fazer

Para calcular uma data no futuro ou no passado em TypeScript, é necessário utilizar a classe `Date` e seus métodos `getDate()`, `getMonth()` e `getFullYear()` para obter os valores atuais de uma data. Em seguida, podemos utilizar o método `setFullYear()` para definir um novo ano, `setMonth()` para definir um novo mês e `setDate()` para definir um novo dia. 

Por exemplo, se queremos calcular uma data 3 anos no futuro, podemos fazer da seguinte maneira:

```TypeScript
let dataAtual = new Date(); // Obtém a data atual
dataAtual.setFullYear(dataAtual.getFullYear() + 3); // Define o ano para 3 anos no futuro
console.log(`Data daqui a 3 anos: ${dataAtual}`); // Saída: Data daqui a 3 anos: Sun Sep 26 2021 15:16:09 GMT-0300 (Horário Padrão de Brasília)
```

Da mesma forma, se queremos calcular uma data 2 meses e 5 dias no passado, podemos utilizar os métodos `setMonth()` e `setDate()`:

```TypeScript
let dataAtual = new Date(); // Obtém a data atual
dataAtual.setMonth(dataAtual.getMonth() - 2); // Define o mês para 2 meses atrás
dataAtual.setDate(dataAtual.getDate() - 5); // Define o dia para 5 dias atrás
console.log(`Data de 2 meses e 5 dias atrás: ${dataAtual}`); // Saída: Data de 2 meses e 5 dias atrás: Thu Jul 16 2020 15:16:09 GMT-0300 (Horário Padrão de Brasília)
```

## Mergulho profundo

Além dos métodos mencionados acima, a classe `Date` também possui outros métodos úteis para realizar cálculos de datas, como por exemplo `getTime()`, que retorna o número de milissegundos desde 1 de janeiro de 1970, e `setTime()`, que permite configurar uma data utilizando esse valor em milissegundos.

Outro conceito importante para entender ao calcular datas é o `Time Zone` (fuso horário), que pode afetar o resultado final dependendo da localização do usuário. Portanto, é importante sempre levar em conta o `Time Zone` ao trabalhar com datas e certificar-se de utilizar o formato UTC ao armazenar dados.

## Veja também

- Documentação oficial do TypeScript sobre a classe `Date`: https://www.typescriptlang.org/docs/handbook/classes.html#classes
- Tutorial sobre manipulação de datas em TypeScript: https://www.educba.com/typescript-date/
- Livro "TypeScript: A Angular Developer's Guide" do autor Yakov Fain - capítulo sobre `Date` e `Time Zone`: https://www.manning.com/books/typescript-angula