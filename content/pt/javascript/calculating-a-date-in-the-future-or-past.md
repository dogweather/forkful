---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Javascript: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser útil em diversas situações, como por exemplo em sistemas de reserva, agendamento de eventos ou até mesmo para mostrar a idade de uma pessoa em um perfil de rede social.

## Como Fazer

Para calcular uma data no futuro ou no passado no Javascript, podemos utilizar o objeto ```Date``` e seus métodos ```getDate()```, ```setDate()``` e ```toLocaleDate()```. Vamos dar uma olhada em algumas situações de uso abaixo:

### Calcular a data de amanhã

Para calcular a data de amanhã, podemos utilizar o método ```getDate()``` para obter o dia atual e o método ```setDate()``` para definir o dia como um dia a mais. Veja o código abaixo:

```Javascript
let data = new Date();
data.setDate(data.getDate() + 1);
console.log(data.toLocaleDateString()); // Output: 16/08/2021
```

### Calcular a data daqui a uma semana

Da mesma forma, podemos utilizar o método ```setDate()``` para definir a data daqui a uma semana. Veja o exemplo abaixo:

```Javascript
let data = new Date();
data.setDate(data.getDate() + 7);
console.log(data.toLocaleDateString()); // Output: 22/08/2021
```

### Calcular a idade de uma pessoa

Podemos utilizar o método ```toLocaleDateString()``` para obter a data atual e o método ```getFullYear()``` para obter o ano atual. Com isso, podemos fazer o cálculo da idade de uma pessoa. Veja o código abaixo:

```Javascript
let dataNascimento = new Date(1990, 0, 1); // 01/01/1990
let dataAtual = new Date();
let idade = dataAtual.getFullYear() - dataNascimento.getFullYear();
console.log(`A idade atual é de ${idade} anos.`); // Output: A idade atual é de 31 anos.
```

## Deep Dive

Além dos métodos mencionados acima, o objeto ```Date``` também possui outros métodos que podem ser úteis ao lidar com datas no Javascript, como por exemplo o método ```getTime()``` para obter o tempo em milissegundos da data, o método ```getMonth()``` para obter o mês da data e o método ```getDay()``` para obter o dia da semana.

Também é importante lembrar que o objeto ```Date``` armazena as datas no fuso horário do computador, por isso pode ser necessário iterar com o objeto e converter as datas para o fuso horário da sua região.

## Veja Também

- [Documentação do objeto Date no MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutorial sobre manipulação de datas no Javascript](https://blog.geekhunter.com.br/manipulacao-de-datas-e-horarios-no-javascript/)