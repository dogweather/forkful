---
title:    "Java: Calculando uma data no futuro ou passado"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser útil em diversas situações. Por exemplo, em um sistema de reservas, pode ser necessário calcular a data de check-out com base na data de check-in e na duração da reserva. Ou, em um aplicativo de calendário, é importante poder exibir datas de eventos futuros ou passados.

## Como fazer

Para calcular datas no futuro ou no passado em Java, podemos utilizar a classe `Calendar` e seus métodos `add` e `getTime`. Veja um exemplo abaixo:

```Java
// Criando uma instância da classe Calendar e definindo a data atual
Calendar calendar = Calendar.getInstance();
calendar.set(2021, Calendar.JULY, 27);

// Adicionando 10 dias à data atual
calendar.add(Calendar.DAY_OF_MONTH, 10);

// Obtendo a data resultante
Date futureDate = calendar.getTime();
System.out.println(futureDate);
```

O código acima irá resultar em `2021-08-06`, já que 10 dias foram adicionados à data inicial. Para calcular uma data no passado, basta utilizar um valor negativo no método `add`.

## Mergulho profundo

O método `add` da classe `Calendar` nos permite adicionar ou subtrair um determinado valor de um campo de data específico, como dias, meses ou anos. Além disso, também podemos utilizar o método `set` para definir uma data inicial e, posteriormente, usar o método `get` para obter valores específicos, como o dia, mês ou ano.

Também é importante mencionar que, a partir do Java 8, temos a classe `LocalDate` do pacote `java.time` que torna a manipulação de datas ainda mais fácil e intuitiva. Ao invés de utilizar o método `add` da classe `Calendar`, podemos simplesmente utilizar o método `plus` da classe `LocalDate` para adicionar ou subtrair valores de datas.

## Veja também

- [Documentação oficial da classe Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Documentação oficial da classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial de manipulação de datas em Java](https://www.baeldung.com/java-date-time)
- [Vídeo explicativo sobre a classe LocalDate](https://www.youtube.com/watch?v=RjJYcW547bY)