---
title:                "Convertendo uma data em uma string"
aliases: - /pt/typescript/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:58.206352-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma data em uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converter uma data em uma string trata-se de transformar o objeto Date em um texto legível. Programadores fazem isso para exibir datas de forma compreensível para usuários ou para preparar informações para armazenamento e transferência.

## How to:
TypeScript oferece métodos nativos para converter datas em strings. Vamos ver alguns exemplos:
```TypeScript
const dataAtual: Date = new Date();

// Conversão padrão para string
console.log(dataAtual.toString()); // "Wed Apr 05 2023 14:55:26 GMT+0000 (Coordinated Universal Time)"

// Conversão para string no formato ISO
console.log(dataAtual.toISOString()); // "2023-04-05T14:55:26.000Z"

// Conversão para string com data local
console.log(dataAtual.toLocaleDateString()); // "04/05/2023"

// Conversão para string com hora local
console.log(dataAtual.toLocaleTimeString()); // "14:55:26"

// Customizando formato com Intl.DateTimeFormat
const formatador = new Intl.DateTimeFormat('pt-BR', {
    year: 'numeric', month: '2-digit', day: '2-digit',
    hour: '2-digit', minute: '2-digit', second: '2-digit'
});
console.log(formatador.format(dataAtual)); // "05/04/2023 14:55:26"
```

## Deep Dive
A necessidade de converter datas em strings remonta ao início da computação. Datas são armazenadas como números (timestamps), mas isso não é prático para leitura humana; daí a necessidade de conversão.

Outro método inclui a utilização de bibliotecas externas como `Moment.js` ou `date-fns`, que oferecem mais flexibilidade e opções de fuso horário. Todavia, com a evolução dos objetos `Date` e `Intl` do JavaScript, muitas vezes as funcionalidades nativas são suficientes.

Cada navegador pode implementar os métodos de conversão de forma ligeiramente diferente, especialmente com `toLocaleDateString()` e `toLocaleTimeString()`, então é importante testar o comportamento no ambiente alvo.

No TypeScript, ao trabalhar com datas, tipamos as variáveis como `Date` e o intellisense nos ajuda a descobrir os métodos disponíveis.

## See Also
- [MDN Web Docs - Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Web Docs - Intl](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Intl)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
- [date-fns – Modern JavaScript date utility library](https://date-fns.org/)
- [Moment.js – Parse, validate, manipulate, and display dates in JavaScript](https://momentjs.com/)
