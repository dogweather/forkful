---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Java: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular datas futuras ou passadas em Java?

Calcular datas futuras ou passadas é um recurso útil em aplicações Java que precisam lidar com agendamentos, aniversários ou outras situações temporais. Essa funcionalidade permite que os programadores gerenciem de forma eficiente as datas e horários em suas aplicações, evitando erros e tornando o código mais organizado.

## Como fazer em Java?

Para calcular datas futuras ou passadas em Java, você precisará da classe `Calendar` e do método `add()`. Primeiro, é necessário definir uma instância de `Calendar` com a data atual, utilizando o método `getInstance()`, e em seguida, é possível utilizar o método `add()` para adicionar um número de dias, meses ou anos à data atual. Veja um exemplo:

```Java
import java.util.Calendar;

Calendar hoje = Calendar.getInstance();
hoje.add(Calendar.DAY_OF_MONTH, 7);
System.out.println(hoje.getTime());
```

Esse código irá adicionar 7 dias à data atual e imprimir o resultado na tela.

Para calcular uma data passada, basta utilizar um número negativo no método `add()`. Por exemplo, se quisermos saber qual foi a data há 1 mês atrás, podemos fazer o seguinte:

```Java
import java.util.Calendar;

Calendar hoje = Calendar.getInstance();
hoje.add(Calendar.MONTH, -1);
System.out.println(hoje.getTime());
```

Este código irá subtrair 1 mês da data atual e imprimir o resultado na tela.

É importante lembrar que, ao adicionar ou subtrair meses e anos, a data resultante pode ser diferente da esperada, devido a diferenças nos números de dias dos diferentes meses e anos. Por isso, é recomendável utilizar o método `set()` para definir as datas exatas antes de adicionar ou subtrair valores.

## Deep Dive

O método `add()` da classe `Calendar` segue o calendário gregoriano, que é o padrão internacionalmente reconhecido. No entanto, ainda é possível lidar com outros calendários, como o calendário asiático, utilizando a classe `PersianCalendar`. Além disso, também é possível adicionar ou subtrair valores específicos, como horas, minutos e segundos, utilizando os campos de calendário correspondentes, como `Calendar.HOUR`, `Calendar.MINUTE` e `Calendar.SECOND`.

## Veja também

- [Documentação oficial da classe Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Exemplos práticos de cálculo de datas em Java](https://www.codejava.net/java-se/date/how-to-add-subtract-days-to-current-date-in-java)
- [Tutorial de cálculo de datas em diferentes calendários em Java](https://www.baeldung.com/java-calendars)