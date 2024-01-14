---
title:                "TypeScript: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string

Converter uma data em uma string é uma tarefa comum em programação TypeScript. Isso permite que os desenvolvedores apresentem a data de forma legível para os usuários em seus aplicativos ou simplesmente armazenem a data em um formato mais fácil de manipular. Aprender a converter uma data em uma string pode tornar o seu código mais eficiente e profissional.

## Como fazer

Para converter uma data em uma string em TypeScript, existem algumas opções. A primeira é usar o método `toDateString()` para converter a data em uma string legível por humanos. Veja um exemplo abaixo:

```TypeScript
let data = new Date();
console.log(data.toDateString()); // Saída: Fri Jul 31 2020
```

Outra opção é usar o método `toLocaleDateString()` para converter a data em uma string com base nas configurações regionais do usuário. Veja um exemplo abaixo:

```TypeScript
let data = new Date();
console.log(data.toLocaleDateString()); // Saída: 31/07/2020
```

É possível especificar as opções de idioma e formato de data ao usar o `toLocaleDateString()`. Por exemplo, para exibir a data em português, você pode usar o código de idioma `'pt-br'` e especificar o formato de data como `'dd/MM/yyyy'`. Veja um exemplo abaixo:

```TypeScript
let data = new Date();
let opcoes = {day: '2-digit', month: '2-digit', year: 'numeric'};
console.log(data.toLocaleDateString('pt-br', opcoes)); // Saída: 31/07/2020
```

## Deep Dive

Ao converter uma data em uma string, é importante levar em consideração a manipulação do fuso horário. O objeto `Date` em JavaScript possui uma função `getTimezoneOffset()` que pode ser usada para obter a diferença de tempo entre o horário local e o UTC. Isso pode ser útil ao exibir a data para usuários em diferentes fusos horários.

Também é importante notar que o método `toLocaleDateString()` pode ser afetado pelas configurações regionais do navegador. Por exemplo, se o navegador do usuário estiver configurado para uma região diferente do Brasil, a data será exibida de acordo com as configurações desse país.

## Veja Também

- [Documentação oficial do TypeScript sobre o objeto Date](https://www.typescriptlang.org/docs/handbook/declaration-files/Destructuring.html#destructuring)
- [Guia do desenvolvedor do Mozilla sobre o objeto Date em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)