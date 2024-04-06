---
date: 2024-01-20 17:31:22.608370-07:00
description: "Como Fazer: **Sa\xEDda de exemplo:**."
lastmod: '2024-04-05T21:53:46.809342-06:00'
model: gpt-4-1106-preview
summary: "**Sa\xEDda de exemplo:**."
title: Calculando uma data no futuro ou passado
weight: 26
---

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
