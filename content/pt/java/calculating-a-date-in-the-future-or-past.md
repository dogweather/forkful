---
title:                "Java: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por quê

A calculadora de datas pode ser uma ferramenta útil em muitas situações, como planejar viagens, acompanhar prazos ou simplesmente para ter uma noção do tempo. Além disso, aprender a programar essa funcionalidade pode aprimorar suas habilidades em Java e torná-lo um programador mais completo.

# Como fazer

Para calcular uma data futura ou passada em Java, usaremos a classe `Calendar` e o método `add()`, que permite adicionar ou subtrair uma determinada quantidade de tempo a uma data específica. Usaremos como exemplo a adição de 2 dias à data atual:

```
import java.util.Calendar;

public class CalculadoraDatas {
  public static void main(String[] args) {
    // criando o objeto Calendar com a data atual
    Calendar dataAtual = Calendar.getInstance();
    System.out.println("Data atual: " + dataAtual.getTime());

    // adicionando 2 dias à data atual
    dataAtual.add(Calendar.DATE, 2);
    System.out.println("Data futura: " + dataAtual.getTime());
  }
}
```

A saída desse código seria:

```
Data atual: Sun Oct 10 16:30:20 BRT 2021
Data futura: Tue Oct 12 16:30:20 BRT 2021
```

Também é possível subtrair uma quantidade de tempo, basta usar um número negativo no segundo parâmetro do método `add()`. Por exemplo, se quisermos calcular 1 mês antes da data atual:

```
dataAtual.add(Calendar.MONTH, -1);
```

# Aprofundando

Além dos parâmetros `DATE` e `MONTH`, a classe `Calendar` possui outros campos que podem ser usados para adicionar ou subtrair tempo, como `HOUR`, `MINUTE`, `SECOND`, entre outros. Além disso, é possível usar as classes `Date` e `SimpleDateFormat` para formatar melhor a saída da data.

Existe também a possibilidade de usar a classe `LocalDate` do Java 8 em diante, que possui métodos mais intuitivos e simplificados para calcular datas. Por exemplo, para adicionar 2 anos à data atual:

```
import java.time.LocalDate;

public class CalculadoraDatas {
  public static void main(String[] args) {
    // criando o objeto LocalDate com a data atual
    LocalDate dataAtual = LocalDate.now();
    System.out.println("Data atual: " + dataAtual);

    // adicionando 2 anos à data atual
    LocalDate dataFutura = dataAtual.plusYears(2);
    System.out.println("Data futura: " + dataFutura);
  }
}
```

A saída seria:

```
Data atual: 2021-10-10
Data futura: 2023-10-10
```

# Veja também

- Documentação oficial do Java sobre a classe `Calendar`: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Calendar.html
- Documentação oficial do Java sobre a classe `LocalDate`: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html
- Exemplos de uso da classe `LocalDate`: https://www.baeldung.com/java-localdate