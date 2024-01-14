---
title:                "Javascript: Cálculo de uma data no futuro ou passado"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser útil em muitos casos, como em um sistema de reservas ou planejamento de eventos. É importante saber como realizar esse cálculo de forma eficiente e precisa.

## Como fazer

Para calcular uma data no futuro ou no passado, é necessário usar a classe `Date` do Javascript. Esta classe possui diversos métodos que permitem manipular datas de forma flexível e poderosa.

Um exemplo simples de como calcular uma data no futuro seria adicionar um determinado número de dias à data atual. Veja como isso pode ser feito:

```Javascript
// Declara a variável com a data atual
let dataAtual = new Date();
// Define o número de dias a serem adicionados (neste caso, 7 dias)
let dias = 7;
// Adiciona os dias à data atual
dataAtual.setDate(dataAtual.getDate() + dias);
```

Agora, se quisermos calcular uma data no passado, basta subtrair o número de dias desejado ao invés de adicioná-lo. Veja:

```Javascript
// Declara a variável com a data atual
let dataAtual = new Date();
// Define o número de dias a serem subtraídos (neste caso, 14 dias)
let dias = 14;
// Subtrai os dias da data atual
dataAtual.setDate(dataAtual.getDate() - dias);
```

É importante lembrar que essa operação altera diretamente o objeto `Date` da variável `dataAtual`. Se for necessário manter a data atual sem alterações, é possível criar uma cópia da data antes de realizar o cálculo.

## Mergulho profundo

Além de adicionar ou subtrair dias, a classe `Date` possui muitos outros métodos que permitem calcular datas no futuro ou no passado de diversas formas. É possível, por exemplo, obter o primeiro dia do próximo ou do último mês, ou ainda verificar se uma determinada data já passou.

Além disso, é importante prestar atenção nos formatos de data e hora utilizados pelo Javascript, pois eles podem variar de acordo com a localização e configurações do sistema. Para isso, é possível utilizar os métodos `toLocaleString()` e `toLocaleDateString()` para obter a data e hora em formatos específicos.

Para saber mais sobre como se aprofundar no cálculo de datas futuras ou passadas, consulte a documentação oficial do Javascript sobre a classe `Date` e seus métodos.

## Veja também

- [Documentação oficial - classe Date (em inglês)](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutorial para manipulação de datas em Javascript (em português)](https://www.devmedia.com.br/manipulando-datas-em-javascript/31185)