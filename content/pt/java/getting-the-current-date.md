---
title:                "Obtendo a data atual"
html_title:           "Java: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Saber a data atual é algo bastante útil em programação, seja para registrar eventos, calcular intervalos de tempo ou exibir informações para o usuário. Felizmente, em Java, existe uma maneira simples de obter a data atual.

## Como fazer

Para obter a data atual em Java, podemos utilizar a classe `LocalDate` do pacote `java.time`. Veja o exemplo abaixo:

```Java
import java.time.LocalDate;

public class Main {
  public static void main(String[] args) {
    LocalDate dataAtual = LocalDate.now();
    System.out.println("A data atual é: " + dataAtual);
  }
}
```
Output: A data atual é: 2020-10-07

Neste exemplo, utilizamos o método `now()` da classe `LocalDate` para obter a data atual e armazenamos o resultado em uma variável do tipo `LocalDate`. Em seguida, imprimimos a variável utilizando o método `println()` da classe `System`.

Mas e se quisermos exibir a data formatada de outra maneira? Podemos utilizar o método `format()` da classe `DateTimeFormatter` para especificar um formato personalizado. Veja o exemplo abaixo:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
  public static void main(String[] args) {
    LocalDate dataAtual = LocalDate.now();
    DateTimeFormatter formatador = DateTimeFormatter.ofPattern("dd/MM/yyyy");
    System.out.println("A data atual é: " + dataAtual.format(formatador));
  }
}
```
Output: A data atual é: 07/10/2020

Neste caso, utilizamos o método `ofPattern()` da classe `DateTimeFormatter` para especificar o formato desejado como parâmetro. Em seguida, utilizamos o método `format()` da mesma classe para formatar a data atual de acordo com o formato especificado.

## Aprofundando

Se você quiser saber mais sobre como trabalhar com datas em Java, vale a pena conhecer outras classes do pacote `java.time`, como `LocalTime` e `LocalDateTime`, que permitem trabalhar com horas e datas e horas respectivamente.

É possível, por exemplo, obter a data e hora atual com o método `now()` da classe `LocalDateTime` e formatar o resultado para exibir somente a hora utilizando o método `format()` com o formato "HH:mm:ss".

## Veja também

- [Documentação da classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial do pacote java.time](https://www.baeldung.com/java-8-date-time-intro)
- [Referência sobre formatação de datas e horas em Java](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)