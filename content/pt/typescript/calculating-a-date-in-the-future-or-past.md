---
title:    "TypeScript: Calculando uma data no futuro ou passado"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por que calcular datas no futuro ou passado é útil?

Calcular datas no futuro ou passado pode ser útil em diversas situações, como programação de tarefas, agendamento de compromissos ou simplesmente para planejar eventos futuros. Além disso, pode ser útil para verificar e comparar datas com rapidez e precisão.

## Como fazer isso em TypeScript

Para calcular datas no futuro ou passado em TypeScript, podemos utilizar a classe `Date`. Ela permite trabalhar com datas e horários de forma simples e eficiente. Para isso, basta seguir os seguintes passos:

1. Criar uma nova instância da classe `Date` com a data desejada (atual, futura ou passada);
2. Utilizar os métodos `setFullYear()`, `setMonth()` e `setDate()` para definir a data desejada;
3. Utilizar o método `getTime()` para obter o timestamp da data e, em seguida, manipular esse valor para obter a data no futuro ou passado desejado.

Veja um exemplo de código para calcular a data de 10 dias no futuro a partir da data atual:

```
// Criando uma instância da classe Date com a data atual
let dataAtual = new Date();

// Definindo a data para 10 dias no futuro
dataAtual.setDate(dataAtual.getDate() + 10);

// Obtendo o timestamp da nova data
let timestamp = dataAtual.getTime();

// Criando uma nova instância com a nova data
let dataFutura = new Date(timestamp);

// Imprimindo a data futura
console.log(dataFutura);    // Saída: Ter, 21 Set 2021 17:17:36 GMT-0300 (Horário Padrão de Brasília)
```

## Aprofundando-se no assunto

Ao trabalhar com datas no futuro ou passado, é importante ter em mente a função `getTimezoneOffset()` da classe `Date`. Ela permite obter o deslocamento do fuso horário atual em relação ao UTC (Tempo Universal Coordenado) em minutos. Com esse valor, é possível converter a data para o fuso horário desejado e assim, obter a data no local correto.

Além disso, é importante lembrar que a classe `Date` também possui diversos métodos para manipular datas e horários, como `setHours()`, `setMinutes()`, `setSeconds()`, entre outros. Combinando esses métodos, é possível criar uma lógica mais complexa para calcular datas no futuro ou passado.

# Veja também

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Manipulando datas em JavaScript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)