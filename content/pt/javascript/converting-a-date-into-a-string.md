---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Conversão de Datas para Strings em JavaScript

## O Que e Por Quê?

Em programação, converter uma data para uma string significa transformar uma instância do objeto Date em uma representação textual. Isso é útil para exibir datas de forma compacta e legível para humanos ou para a armazenagem em formatos textuais, como JSON.

## Como Fazer:

Veja exemplos claros de como fazer isso em JavaScript:

```Javascript
let dataAtual = new Date();
let stringData = dataAtual.toString();
console.log(stringData);
```

O código acima vai exibir uma string semelhante a: "Wed Apr 07 2021 11:42:16 GMT+0200 (Central European Summer Time)".

Para ter mais controle sobre o formato da data, você pode usar a função toLocaleDateString():

```Javascript
let dataAtual = new Date();
let stringData = dataAtual.toLocaleDateString('pt-BR');
console.log(stringData);
```

Nesse caso, a saída seria "07/04/2021", que é a forma padronizada de exibir datas no Brasil.

## Deep Dive

A capacidade de converter datas em strings é uma característica do JavaScript desde sua criação em 1995. No entanto, a função toLocaleDateString() só foi adicionada posteriormente, permitindo maior controle sobre a formatação.

Alternativamente, você pode usar bibliotecas como Moment.js ou Date-fns para manipulação mais avançada de datas. Essas bibliotecas podem oferecer mais recursos do que o JavaScript nativo, como formato personalizado, suporte a fusos horários e muito mais.

Detalhe importante: as funções nativas do JavaScript para formatação de data são implementadas de acordo com as configurações do ambiente de execução. Ou seja, a saída pode variar dependendo das configurações do usuário ou do servidor onde o JavaScript está sendo executado.

## Veja Também

Para aprofundar seus conhecimentos em datas e horas em JavaScript, confira os seguintes links:

- Documentação oficial do Mozilla sobre o Objeto Date: [MDN Web Docs - Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Tutorial detalhado sobre datas em JavaScript: [JavaScript.Info - Dates](https://javascript.info/date)
- Documentação das bibliotecas Moment.js e Date-fns:
    - [Moment.js](https://momentjs.com/docs/)
    - [Date-fns](https://date-fns.org/)