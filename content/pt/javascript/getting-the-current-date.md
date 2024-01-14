---
title:    "Javascript: Obtendo a data atual."
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que utilizar programação Javascript?

Se você é um programador ou está começando a aprender a linguagem, provavelmente já se deparou com a necessidade de obter a data atual em seus projetos. Isso pode ser útil para mostrar a data atual em seu site ou aplicativo, criar recursos que dependam da data ou até mesmo registrar as datas de atividades específicas. Felizmente, a programação em Javascript torna isso uma tarefa simples.

## Como obter a data atual com Javascript

Para obter a data atual em seu projeto Javascript, você pode usar o objeto `Date`. Com ele, você pode acessar vários métodos e propriedades relacionados à data. Vamos dar uma olhada em alguns exemplos de código:

```
// Criando um objeto Date
var today = new Date();

// Obtendo a data atual em formato de string
var currentDate = today.toDateString();
console.log(currentDate); // Saída: Fri Mar 05 2021

// Obtendo o dia da semana atual
var currentDay = today.getDay();
console.log(currentDay); // Saída: 5 (sendo 0 = domingo, 1 = segunda, ...)
```

Você também pode personalizar o formato da data utilizando os métodos `getMonth()` e `getFullYear()`. Por exemplo:

```
// Obtendo o mês e ano atual em formato de string
var currentMonth = today.getMonth() + 1; // Os meses começam em 0
var currentYear = today.getFullYear();
console.log(currentMonth + "/" + currentYear); // Saída: 3/2021
```

Além disso, você também pode adicionar ou subtrair dias, meses ou anos à data atual utilizando os métodos `setDate()`, `setMonth()` e `setFullYear()`. Por exemplo:

```
// Adicionando 10 dias à data atual
today.setDate(today.getDate() + 10);
console.log(today.toDateString()); // Saída: Mon Mar 15 2021
```

## Aprofundando-se na obtenção da data atual

O objeto `Date` também possui métodos para obter a data e horário local, além de permitir definir o fuso horário. Além disso, é importante lembrar que a data atual é baseada no fuso horário configurado no dispositivo do usuário. Portanto, é sempre recomendável converter a data e hora para o fuso horário do servidor antes de armazená-las no banco de dados, por exemplo.

Outra dica importante é que, quando comparar datas em Javascript, é necessário utilizar o método `getTime()` para converter as datas em milissegundos, caso contrário, a comparação será feita com base na diferença do fuso horário do dispositivo do usuário.

## Veja também

- [Documentação do objeto Date em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Converter fuso horário em Javascript](https://www.w3schools.com/js/js_date_methods.asp)