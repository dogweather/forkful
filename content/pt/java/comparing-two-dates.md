---
title:                "Comparando duas datas"
html_title:           "Java: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# O que & Por quê?
Comparar duas datas é uma tarefa comum para os programadores. Essa ação envolve comparar duas datas e determinar qual é a mais recente ou se são iguais. Os programadores fazem isso para validar entrada de dados, calcular duração de eventos, e para outras tarefas relacionadas à manipulação de datas.

# Como fazer:
Para comparar duas datas em Java, utilize o método `compareTo()` da classe `LocalDate`. O método retornará um número negativo se a primeira data for anterior à segunda, um número positivo se for posterior e zero se forem iguais. Veja um exemplo abaixo:

```java
LocalDate data1 = LocalDate.of(2021, 5, 1);
LocalDate data2 = LocalDate.now();
int resultado = data1.compareTo(data2);

if (resultado < 0) {
    System.out.println("Data1 é anterior a Data2");
} else if (resultado > 0) {
    System.out.println("Data2 é anterior a Data1");
} else {
    System.out.println("Data1 e Data2 são iguais");
}
```

Output: `Data1 é anterior a Data2`

# Mergulho profundo:
Comparar datas é uma tarefa complexa devido às diferentes maneiras de se representar datas em diferentes culturas, como o formato de dia/mês/ano utilizado no Brasil, e o formato de mês/dia/ano utilizado nos Estados Unidos. Além disso, existem bibliotecas de terceiros que podem ser utilizadas para comparar dias específicos da semana, como feriados ou finais de semana.

# Veja também:
- Documentação oficial do método `compareTo()` em Java: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#compareTo-java.time.LocalDate-
- Outras opções para manipulação de datas em Java: https://javarevisited.blogspot.com/2016/12/how-to-compare-two-dates-in-java.html