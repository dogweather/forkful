---
title:                "Comparando duas datas"
aliases:
- pt/java/comparing-two-dates.md
date:                  2024-01-20T17:33:07.367530-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparando duas datas"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Comparar duas datas significa verificar se são iguais, ou determinar qual vem antes ou depois no tempo. Programadores fazem isso frequentemente para validações, agendamentos, ou para controlar períodos em aplicações.

## Como fazer:

```java
import java.time.LocalDate;
import java.time.Month;
import java.time.temporal.ChronoUnit;

public class ComparacaoDeDatas {
    public static void main(String[] args) {
        LocalDate dataInicio = LocalDate.of(2023, Month.JANUARY, 1);
        LocalDate dataFim = LocalDate.of(2023, Month.DECEMBER, 31);

        // Comparação simples: é depois?
        boolean isAfter = dataInicio.isAfter(dataFim);
        System.out.println("Data de início é depois da data de fim? " + isAfter); // false

        // Comparação simples: é antes?
        boolean isBefore = dataInicio.isBefore(dataFim);
        System.out.println("Data de início é antes da data de fim? " + isBefore); // true

        // Comparação de igualdade
        boolean isEqual = dataInicio.isEqual(dataFim);
        System.out.println("As datas são iguais? " + isEqual); // false

        // Diferença em dias
        long daysBetween = ChronoUnit.DAYS.between(dataInicio, dataFim);
        System.out.println("Dias entre as datas: " + daysBetween); // 364
    }
}
```

## Visão Detalhada

Antigamente, comparar datas em Java era mais complicado e impreciso, usando `java.util.Date` e `SimpleDateFormat`. Essas classes podiam ter problemas com threads e usavam índices base 0 para meses, o que confundia. Desde o Java 8, a API `java.time` (Joda-Time inspirou) simplificou essa tarefa com `LocalDate`, `LocalTime`, e `LocalDateTime`.

Existem alternativas como a `Calendar` (da velha API), mas a nova API é mais intuitiva e segura em relação a threads. Detalhes de implementação da nova API incluem imutabilidade (objetos não mudam, você cria novos) e clareza de métodos (`isBefore`, `isAfter`, `isEqual`).

## Veja Também

- Documentação oficial da Oracle sobre a API java.time: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Tutorial da API de data e hora do Java: [https://www.baeldung.com/java-8-date-time-intro](https://www.baeldung.com/java-8-date-time-intro)
