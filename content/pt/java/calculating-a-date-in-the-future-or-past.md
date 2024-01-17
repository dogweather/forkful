---
title:                "Calculando uma data no futuro ou passado."
html_title:           "Java: Calculando uma data no futuro ou passado."
simple_title:         "Calculando uma data no futuro ou passado."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que e Porque?

Calcular uma data no passado ou no futuro é uma tarefa comum para programadores. Isso envolve determinar uma data base e adicionar ou subtrair um determinado número de dias, semanas, meses, ou anos para obter uma nova data. Isso é útil para automatizar tarefas ou criar funcionalidades como notificações ou agendamentos em sistemas.

## Como Fazer:

Veja abaixo um exemplo de código em Java para calcular uma data no futuro, adicionando 30 dias a partir de uma data base:

```Java
import java.util.Calendar;
import java.util.Date;

public class CalcularData {
    public static void main(String[] args) {
        
        Calendar calendar = Calendar.getInstance();
        
        // usar a data atual como a base
        Date baseDate = calendar.getTime();
        
        // adicionar 30 dias
        calendar.add(Calendar.DAY_OF_MONTH, 30);
        
        // armazenar a nova data em uma variável
        Date futureDate = calendar.getTime();
        
        // imprimir a nova data
        System.out.println("A data daqui a 30 dias será: " + futureDate);
    }
}
```

A saída do programa será:

```
A data daqui a 30 dias será: Sat Aug 07 11:59:35 BRT 2021
```

## Mergulho Profundo:

Há várias maneiras de calcular uma data no futuro ou passado em Java, como usando as classes `Calendar` ou `LocalDate` da API de data e hora padrão. Esta última é recomendada a partir da versão 8 da linguagem. Além disso, é importante considerar a questão do fuso horário ao lidar com datas e horários em sistemas distribuídos ou globais.

## Veja Também:

- Documentação oficial do Java sobre a API de data e hora: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Tutoriais e exemplos sobre como lidar com datas em Java: https://www.baeldung.com/java-date-time