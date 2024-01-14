---
title:    "Java: Obtendo a data atual"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que obter a data atual?

Ao desenvolver em Java, é comum precisar obter a data atual para diferentes propósitos, como registrar eventos, criar timestamps ou simplesmente mostrar a data no formato correto. Felizmente, existem várias maneiras de obter a data atual em Java.

## Como fazer:

Para obter a data atual em Java, podemos utilizar a classe `LocalDate` do pacote `java.time`. Veja um exemplo simples:

```java
LocalDate dataAtual = LocalDate.now();
System.out.println(dataAtual);
```

Ao executar esse código, o resultado será algo como `2021-05-25`. Podemos também formatar a data da maneira desejada, utilizando a classe `DateTimeFormatter`. Por exemplo:

```java
DateTimeFormatter formatoPersonalizado = DateTimeFormatter.ofPattern("dd/MM/yyyy");
String dataFormatada = dataAtual.format(formatoPersonalizado);
System.out.println(dataFormatada);
```

O resultado será `25/05/2021`, no formato dia/mês/ano.

Para obter também a hora e o fuso horário, podemos utilizar a classe `LocalDateTime` da mesma forma que utilizamos `LocalDate`. Por exemplo:

```java
LocalDateTime dataHoraAtual = LocalDateTime.now();
System.out.println(dataHoraAtual);
```

O resultado será algo como `2021-05-25T13:26:10.123456`, contendo a data, hora e milissegundos.

## Mergulho Profundo:

Além das classes já mencionadas, Java também possui a classe `Calendar`, que permite manipular datas específicas e realizar cálculos com elas. No entanto, é importante notar que `Calendar` é uma classe legada e foi substituída pelas classes `LocalDate` e `LocalDateTime` no Java 8. Portanto, é recomendado utilizá-las ao invés de `Calendar`.

Outra opção é utilizar bibliotecas externas, como o `Joda-Time`, que possui uma API mais amigável e completa para trabalhar com datas e horas em Java.

## Veja também:

- [Documentação oficial - Classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Article "The Date Time API in Java 8" (em inglês)](https://www.baeldung.com/java-8-date-time-intro)
- [Documentação oficial do Joda-Time](https://www.joda.org/joda-time/)