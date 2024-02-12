---
title:                "Analisando uma data a partir de uma string"
aliases: - /pt/java/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:20.408512-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Analisar uma data a partir de uma string envolve converter a representação textual de uma data e hora em um objeto `Date` ou em um objeto `LocalDateTime` mais moderno. Programadores fazem isso para manipular, formatar, comparar ou armazenar datas em um formato padronizado, o que é crucial para aplicações que requerem cálculos de datas, validação ou internacionalização consistente.

## Como Fazer:

### Usando o pacote `java.time` (Recomendado no Java 8 e posteriores):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Saída: 2023-04-30
    }
}
```

### Usando `SimpleDateFormat` (Abordagem Antiga):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // O formato da saída depende do formato padrão do seu sistema
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Usando Bibliotecas de Terceiros (ex.: Joda-Time):
Joda-Time tem sido uma biblioteca de terceiros significativa, mas agora está em modo de manutenção devido à introdução do pacote `java.time` no Java 8. Contudo, para aqueles que usam versões do Java anteriores ao 8, Joda-Time é uma boa escolha.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Saída: 2023-04-30
    }
}
```
Note que, ao trabalhar com datas, sempre esteja ciente das configurações de fuso horário se estiver analisando ou formatando datas-horas, em vez de apenas datas.
