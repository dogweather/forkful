---
title:                "Javascript: Convertendo uma data em uma string."
simple_title:         "Convertendo uma data em uma string."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Muitas vezes, durante o desenvolvimento de um projeto, é necessário trabalhar com datas. Ao imprimir ou exibir essas datas, é importante que elas estejam em um formato legível para os usuários. É aqui que surge a necessidade de converter uma data em uma string. Isso permite que a data seja exibida de forma mais compreensível e personalizável para os usuários.

## Como converter uma data em uma string

Para converter uma data em uma string em Javascript, podemos utilizar o método integrado "toLocaleDateString()" que retorna a data em uma string de acordo com o idioma e configurações de data do usuário. Veja o exemplo abaixo:

```Javascript
let data = new Date();
console.log(data.toLocaleDateString()); // Saída: DD/MM/AAAA (dependendo das configurações do usuário)
```
Este método é muito útil para exibir a data em forma de texto, mas pode ser limitado em questão de personalização. Para isso, podemos utilizar o método "toString()" que nos permite especificar um formato de data personalizado. Veja o exemplo abaixo:

```Javascript
let data = new Date();
console.log(data.toString("MMMM dd, yyyy")); // Saída: Mês DD, AAAA (ex: janeiro 01, 2021)
```

## Aprofundando na conversão de data em string

Ao trabalhar com datas em Javascript, é importante estar ciente dos diferentes formatos de data e como eles podem ser convertidos em strings. Além do método "toString()" e "toLocaleDateString()", existem outras maneiras de realizar essa conversão, como utilizando bibliotecas externas ou criando funções personalizadas.

Além disso, é importante estar atento às diferenças entre os padrões de datas nas diferentes regiões do mundo, pois isso pode afetar a forma como as datas são exibidas para os usuários.

## Veja também

- [Documentação oficial do método toLocaleDateString em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)

- [Artigo sobre conversão de datas em strings em Javascript](https://css-tricks.com/everything-you-ever-wanted-to-know-about-javascript-dates/)

- [Biblioteca Moment.js para manipulação de datas em Javascript](https://momentjs.com/)