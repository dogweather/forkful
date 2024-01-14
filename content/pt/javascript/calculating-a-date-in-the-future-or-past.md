---
title:    "Javascript: Calculando uma data no futuro ou passado"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que

Seja para agendar um evento ou criar uma função de lembrete, calcular uma data no futuro ou no passado é uma tarefa comum em programação. Isso pode ser útil para aplicações de calendário, aplicativos de reserva ou simplesmente para exibir uma data personalizada em um site. Se você está se perguntando como fazer isso em Javascript, este artigo irá te mostrar como.

## Como fazer

Para calcular uma data no futuro ou no passado, precisamos dos seguintes elementos: uma data base, um intervalo de tempo e a operação a ser realizada (adição ou subtração). Podemos usar o objeto `Date()` em Javascript para criar uma data base e a função `setDate()` para alterar a data base de acordo com o intervalo de tempo.

Por exemplo, vamos supor que queremos calcular a data daqui a 7 dias. Podemos fazer isso da seguinte maneira:

````Javascript
// criando uma data base
let dataBase = new Date();

// adicionando 7 dias à data base
dataBase.setDate(dataBase.getDate() + 7);

// exibindo a data final
console.log(dataBase);
````
O código acima irá imprimir a data daqui a 7 dias a partir da data atual. Podemos também subtrair uma determinada quantidade de dias, semanas, meses ou anos da mesma maneira, utilizando a função `setDate()` com valores negativos.

Além disso, podemos também utilizar a função `toLocaleDateString()` para formatar a data de maneira legível, de acordo com a localização do usuário.

## Deep Dive

Em casos mais complexos, como calcular uma nova data com base em uma data pré-existente ou considerando os diferentes números de dias de cada mês, é importante entender como os objetos `Date()` e o método `setDate()` funcionam em conjunto. 

O objeto `Date()` em Javascript é representado por uma única contagem de tempo  em milissegundos, que começa na data `1 de janeiro de 1970 00:00:00 UTC`. A função `setDate()` utiliza essa contagem de tempo para calcular e alterar a data base de acordo com o intervalo especificado.

Devido à complexidade do cálculo de datas, recomenda-se utilizar uma biblioteca de terceiros, como o Moment.js, para facilitar esse processo.

## Veja também

- [Documentação oficial do objeto `Date()` em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Documentação oficial do método `setDate()` em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/setDate)
- [Biblioteca Moment.js para cálculo de datas em Javascript](https://momentjs.com/)