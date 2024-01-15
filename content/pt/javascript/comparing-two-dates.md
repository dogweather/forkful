---
title:                "Comparando duas datas"
html_title:           "Javascript: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

Por que:

Comparar datas é uma tarefa comum na programação, especialmente em projetos que envolvem agendamento, acompanhamento de prazos ou análise de dados temporais. Ao aprender a comparar duas datas em Javascript, você poderá facilitar essas tarefas e tornar seu código mais eficiente.

Como fazer:

```javascript
// Definindo duas datas para comparar
let data1 = new Date("2021-01-01");
let data2 = new Date("2021-02-01");

// Comparando as datas
if (data1 < data2) {
  console.log("A data1 é anterior à data2.");
} else if (data1 > data2) {
  console.log("A data1 é posterior à data2.");
} else {
  console.log("As datas são iguais.");
}

// Saída: A data1 é anterior à data2.
```

Deep Dive:

Para comparar duas datas em Javascript, é importante entender o tipo de dado que o objeto Date armazena. Ele representa uma data e hora específicas, medidos em milissegundos desde 01 de janeiro de 1970, à meia-noite UTC. Isso significa que quando criamos uma nova data, podemos passar tanto uma string com uma data específica quanto os parâmetros correspondentes aos elementos da data (ano, mês, dia, hora, etc.).

Ao comparar duas datas, estamos comparando seus valores em milissegundos, o que nos permite utilizar os operadores de comparação comuns como <, >, <= e >=. Além disso, a classe Date possui métodos úteis para obter informações específicas, como getFullYear(), getMonth(), getDate(), entre outros.

Porém, é importante lembrar que as comparações podem ser afetadas por questões como o fuso horário ou o horário de verão. Por isso, é necessário tomar cuidado ao lidar com o objeto Date e sempre considerar a possibilidade de diferenças de horário.

Veja também:

- Documentação oficial do objeto Date em Javascript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date
- Tutorial sobre comparação de datas em Javascript: https://www.w3schools.com/js/js_dates.asp
- Discussão sobre a complexidade de comparar datas em diferentes linguagens de programação: https://stackoverflow.com/questions/770970/datetime-compare-date-in-javascript