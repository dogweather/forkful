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

## Por que

Se você já trabalhou com datas em um projeto de programação, sabe que em algum momento será necessário convertê-las em formato de texto. Isso pode ser útil para exibir a data em um formato específico, como DD/MM/AAAA ou para uso em bancos de dados. Com o TypeScript, essa tarefa pode ser realizada de forma simples e eficiente.

## Como Fazer

Para converter uma data em uma string usando o TypeScript, você pode utilizar o método `toString()` do objeto Date. Veja um exemplo abaixo:

```TypeScript
let data = new Date(); // Cria uma nova instância do objeto Date, com a data e hora atuais
let dataString = data.toString(); // Converte a data em uma string

console.log(dataString) // Saída: "Wed Feb 23 2022 15:00:00 GMT-0300"
```

Você também pode especificar o formato em que a data será exibida utilizando os métodos `getDate()`, `getFullYear()` e `getMonth()`. Veja o exemplo abaixo:

```TypeScript
let data = new Date(); // Cria uma nova instância do objeto Date, com a data e hora atuais
let dia = data.getDate();
let mes = data.getMonth() + 1; // Precisamos adicionar 1 ao mês, pois o retorno do método começa em 0
let ano = data.getFullYear();

let dataString = dia + "/" + mes + "/" + ano; // Concatenamos as informações para formar a data em texto

console.log(dataString) // Saída: "23/02/2022"
```

## Aprofundando-se

Ao converter uma data em uma string, é importante estar atento ao formato em que ela será exibida. Isso pode variar de acordo com a localização e configuração do seu sistema operacional. Por exemplo, em um sistema com localização em inglês, a data pode ser exibida como "02/23/2022", enquanto em um sistema com localização em português, ela pode ser exibida como "23/02/2022". Além disso, é possível utilizar bibliotecas externas, como o Moment.js, para facilitar a manipulação de datas em JavaScript e TypeScript.

## Veja Também

- Documentação Oficial do TypeScript sobre o objeto Date: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#object-types-with-symbol-named-properties
- Moment.js: https://momentjs.com/
- Manipulando datas com Moment.js: https://levelup.gitconnected.com/manipulating-dates-with-moment-js-a8ca4f9d7017