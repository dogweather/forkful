---
title:    "Java: Calculando uma data no futuro ou no passado"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular uma data futura ou passada pode ser útil em diversas situações, desde agendamento de eventos até cálculos de prazos e validade de documentos. Saber como fazer essa operação em Java pode facilitar o desenvolvimento de programas que necessitem dessa funcionalidade.

## Como Fazer

Para calcular uma data futura ou passada em Java, utilizamos a classe `Calendar`. Primeiro, criamos uma instância dessa classe e definimos a data inicial:

```Java
Calendar calendario = Calendar.getInstance();
calendario.set(2021, Calendar.JANUARY, 1); // 1 de janeiro de 2021
```

Em seguida, podemos adicionar ou subtrair dias, meses ou anos utilizando os métodos `add()` ou `roll()`, respectivamente:

```Java
calendario.add(Calendar.DATE, 10); // adiciona 10 dias à data inicial
calendario.roll(Calendar.MONTH, -2); // subtrai 2 meses da data inicial, mantendo o ano
```

Por fim, para obter a nova data calculada, basta utilizar o método `getTime()` que retorna um objeto `Date`:

```Java
Date novaData = calendario.getTime();
```

Veja o exemplo completo:

```Java
// calcula uma data 1 mês e 10 dias após a data atual
Calendar calendario = Calendar.getInstance();
calendario.add(Calendar.MONTH, 1);
calendario.add(Calendar.DATE, 10);
Date novaData = calendario.getTime();

// imprime a data formatada
SimpleDateFormat formatoData = new SimpleDateFormat("dd/MM/yyyy");
System.out.println("Nova data: " + formatoData.format(novaData)); // 10/09/2021
```

## Deep Dive

A classe `Calendar` possui diversas opções de campos e unidades de tempo que podem ser utilizados para cálculos de datas. É importante ficar atento ao utilizar o método `roll()`, pois ele pode afetar campos que não estão sendo explicitamente alterados.

Outra opção para calcular datas em Java é utilizando a classe `LocalDate` do pacote `java.time`. Ela possui métodos mais intuitivos e simples de usar, além de permitir criar códigos mais legíveis.

## Veja Também

- [Documentação da classe Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Documentação da classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial sobre cálculo de datas em Java](https://www.devmedia.com.br/como-trabalhar-com-datas-em-java/27493)