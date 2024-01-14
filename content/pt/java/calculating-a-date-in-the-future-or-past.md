---
title:                "Java: Calculando uma data no futuro ou no passado"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser extremamente útil em programação, especialmente quando se trata de eventos agendados ou prazos de entrega. Saber como fazer isso em Java pode economizar tempo e esforço no desenvolvimento de aplicativos. 

## Como fazer

Para calcular uma data no futuro ou no passado em Java, é necessário utilizar a classe "Calendar". Primeiro, é preciso instanciar essa classe e definir a data desejada para o cálculo. Por exemplo, para calcular a data de 7 dias a partir de hoje, pode-se utilizar o seguinte código:

```Java
Calendar calendario = Calendar.getInstance();
calendario.add(Calendar.DAY_OF_YEAR, 7);
```

Este código irá adicionar 7 dias à data atual e armazená-la no objeto "calendario". Para obter a data calculada, é necessário utilizar o método "getTime()" e formatá-la de acordo com o padrão desejado. Por exemplo, para exibir a data em formato "dd/MM/yyyy", pode-se utilizar o seguinte código:

```Java
// Formato desejado
SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy");
// Obtém a data calculada
Date dataCalculada = calendario.getTime();
// Formata a data de acordo com o padrão
String dataFormatada = formato.format(dataCalculada);
// Exibe a data formatada
System.out.println(dataFormatada);
```

Ao executar esse código, a saída será "27/03/2021", considerando a data atual como 20/03/2021.

## Deep Dive

Para calcular datas no futuro ou no passado, a classe "Calendar" utiliza o conceito de "Campos", que representam unidades de tempo, como dia, mês, ano, entre outros. Ao utilizar o método "add()", é possível adicionar ou subtrair uma quantidade específica de um campo em relação a uma data. Além disso, é importante levar em consideração o comportamento da classe com datas de anos bissextos e diferentes fuso-horários.

Outra opção para fazer esse cálculo é utilizar a classe "LocalDate", presente no pacote "java.time". Ela oferece métodos mais simples e precisos para lidar com datas e também contém recursos para cálculo de períodos entre datas.

## Veja também

- [Documentação oficial do Java sobre a classe "Calendar"](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Documentação oficial do Java sobre a classe "LocalDate"](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial do CAELUM sobre cálculos de datas em Java](https://www.caelum.com.br/apostila-java-orientacao-objetos/trabalhando-com-datas/#7-3-calcule-uma-data-no-futuro-ou-no-passado)