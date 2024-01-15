---
title:                "Obtendo a data atual"
html_title:           "Javascript: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando em um projeto de programação, pode ser útil saber a data atual. Isso pode ser útil para exibir informações em tempo real, agendar tarefas ou até mesmo para registrar a hora exata em que o código foi executado.

## Como fazer

Para obter a data atual em JavaScript, você pode usar o objeto `Date()` e seus métodos `getDate()`, `getMonth()`, `getFullYear()` para retornar o dia, mês e ano atuais. Veja o exemplo abaixo:

```Javascript
const dataAtual = new Date();
console.log(dataAtual); // output: Mon Oct 11 2021 15:25:40 GMT-0300 (Horário Padrão de Brasília)
console.log(dataAtual.getDate()); // output: 11 -> retorna o dia atual
console.log(dataAtual.getMonth() + 1); // output: 10 -> lembre-se que os meses começam em 0, por isso é necessário adicionar 1
console.log(dataAtual.getFullYear()); // output: 2021 -> retorna o ano atual
```

Você também pode adicionar mais métodos para retornar informações mais específicas, como hora, minutos e segundos.

## Profundidade técnica

Ao usar o objeto `Date()`, é importante lembrar que ele irá retornar a data atual do navegador do usuário, o que significa que pode haver diferenças de fuso horário. Além disso, esse objeto é estático, o que significa que ele não atualizará automaticamente a data e hora, a menos que seja redefinido manualmente.

Você também pode formatar a data de diferentes maneiras usando os métodos `toLocaleString()`, `toLocaleTimeString()` e `toLocaleDateString()`. Esses métodos permitem que você especifique o idioma e a formatação desejados para a data.

## Veja também

- [Documentação do objeto Date em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutorial sobre manipulação de datas em JavaScript](https://www.w3schools.com/js/js_dates.asp)
- [Mais informações sobre o objeto Date](https://javascript.info/date)