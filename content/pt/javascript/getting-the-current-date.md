---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Obtendo a Data Atual em Javascript

## O Que é e Por Que?

Obter a data atual em Javascript é o processo de pegar e exibir a data e a hora correntes. Os programadores fazem isso para rastrear eventos, carimbar datas e horários em arquivos de log e manipular dados baseados em tempo.

## Como Fazer:

Em Javascript, você pode pegar a data atual através do objeto `Date`. Aqui está um exemplo:

```Javascript
var dataAtual = new Date();
console.log(dataAtual);
```

Quando você executa isso, a saída seria algo como isto:

```Javascript
Tue Sep 07 2021 10:45:38 GMT+0800
```

## Mergulho Profundo

Historicamente, a obtenção da data atual em Javascript vem desde a sua criação. A linguagem já tinha o objeto Date incorporado, permitindo aos programadores trabalhar facilmente com datas e horas.

Se você está procurando alternativas, uma opção popular é a biblioteca Moment.js. Ela fornece uma API mais rica para manipular datas e tem melhor suporte para fuso horário.

Na implementação do JavaScript, o objeto Date é baseado no valor do tempo, que é o número de milissegundos desde a meia-noite UTC em 1º de janeiro de 1970. Este é um sistema comum usado na computação chamado Unix Time.

## Veja Também

Para obter mais informações e exemplos de como trabalhar com a data atual em Javascript, você pode consultar os seguintes links:

- Documentação do Mozilla Developer Network sobre o objeto Date: [https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js, uma biblioteca de manipulação de data e hora para Javascript: [https://momentjs.com/](https://momentjs.com/)