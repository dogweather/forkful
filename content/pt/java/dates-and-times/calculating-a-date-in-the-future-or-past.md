---
date: 2024-01-20 17:31:22.608370-07:00
description: "Calcular uma data no futuro ou passado \xE9 simplesmente adicionar ou\
  \ subtrair dias, meses ou anos \xE0 uma data conhecida. Programadores fazem isso\
  \ para\u2026"
lastmod: '2024-02-25T18:49:44.090304-07:00'
model: gpt-4-1106-preview
summary: "Calcular uma data no futuro ou passado \xE9 simplesmente adicionar ou subtrair\
  \ dias, meses ou anos \xE0 uma data conhecida. Programadores fazem isso para\u2026"
title: Calculando uma data no futuro ou passado
---

{{< edit_this_page >}}

## O Que & Por Que?
Calcular uma data no futuro ou passado é simplesmente adicionar ou subtrair dias, meses ou anos à uma data conhecida. Programadores fazem isso para manipular períodos, marcar eventos futuros ou analisar o que aconteceu no passado em aplicações como agendamentos, lembretes ou relatórios históricos.

## Como Fazer:
```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class FuturoEPassado {

    public static void main(String[] args) {
        LocalDate hoje = LocalDate.now();

        // Adicionando 10 dias à data atual
        LocalDate futuro = hoje.plusDays(10);
        System.out.println("Data futura: " + futuro);

        // Subtraindo 30 anos da data atual
        LocalDate passado = hoje.minus(30, ChronoUnit.YEARS);
        System.out.println("Data passada: " + passado);
    }
}
```
**Saída de exemplo:**
```
Data futura: 2023-04-20
Data passada: 1993-04-10
```

## Mergulho Profundo:
Historicamente, o cálculo de datas usava classes como `java.util.Date` e `java.util.Calendar`, que tinham problemas com design e usabilidade. Desde o Java 8, o pacote `java.time`, conhecido como Joda-Time, é a abordagem preferida, oferecendo API's imutáveis e fluentes.

Alternativas incluem bibliotecas de terceiros como Joda-Time ou o Apache Commons Lang. Embora essas ainda sejam opções válidas, a API padrão `java.time` é robusta o suficiente para a maioria dos casos.

Detalhes de implementação envolvem entender a classe `LocalDate` para datas, `LocalTime` para horas, e `LocalDateTime` para ambos. Métodos como `plus` e `minus` são usados para cálculos diretos. Lembre-se dos ajustes de tempo: diferentes unidades de tempo (como `ChronoUnit.DAYS`) e especificidades de fuso horário se calculando timestamps.

## Veja Também:
- [Documentação oficial da API Java Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutorial Oracle sobre a Data e Hora](https://docs.oracle.com/javase/tutorial/datetime/)
- [Joda-Time](https://www.joda.org/joda-time/)
- [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)
