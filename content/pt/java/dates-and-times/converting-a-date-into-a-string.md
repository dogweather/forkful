---
date: 2024-01-20 17:36:46.168370-07:00
description: "Converter uma data em string \xE9 o processo de transformar a representa\xE7\
  \xE3o de data e hora do seu formato original, geralmente num\xE9rico ou de objeto,\
  \ para\u2026"
lastmod: '2024-03-13T22:44:46.467319-06:00'
model: gpt-4-1106-preview
summary: "Converter uma data em string \xE9 o processo de transformar a representa\xE7\
  \xE3o de data e hora do seu formato original, geralmente num\xE9rico ou de objeto,\
  \ para\u2026"
title: Convertendo uma data em uma string
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data em string é o processo de transformar a representação de data e hora do seu formato original, geralmente numérico ou de objeto, para uma sequência de caracteres legíveis. Os programadores fazem isso para formatar datas de maneiras personalizadas, para armazenamento, exibição para usuários ou para interoperabilidade entre sistemas e APIs.

## Como Fazer:

Para converter uma data em String em Java, você pode usar a classe `java.time.format.DateTimeFormatter` junto com as classes de data e hora da API `java.time`. Aqui está um exemplo:

```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class DateFormatExample {
    public static void main(String[] args) {
        LocalDateTime now = LocalDateTime.now();
        
        // Formatar a data atual no padrão dd-MM-yyyy
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");
        String formattedDate = now.format(formatter);
        
        System.out.println("Data formatada: " + formattedDate);
    }
}
```

Saída de exemplo:

```
Data formatada: 15-03-2023
```

## Aprofundando

Historicamente, em Java, a manipulação de datas e horas foi uma jornada, começando com as classes `java.util.Date` e `java.util.Calendar`, que eram notoriamente difíceis de usar. Em Java 8, a API `java.time` foi introduzida como uma melhoria significativa, inspirada na biblioteca Joda-Time, e é a maneira recomendada de se trabalhar com datas desde então.

Além do `DateTimeFormatter`, existem alternativas como a classe `SimpleDateFormat`, que faz parte da API antiga e ainda é usada em código legado, mas é notoriamente menos segura por não ser thread-safe e também um pouco menos intuitiva.

Quando você está implementando a conversão de datas em strings, é importante ter em mente as questões de localização e fuso horário, que podem ser configuradas nos formatadores. O `DateTimeFormatter` pode ser usado com diferentes contextos de localização para adequar a apresentação de datas a linguagens e culturas específicas.

## Veja Também

- Documentação oficial da API `java.time`: [DateTimeFormatter](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/format/DateTimeFormatter.html)
- Guia para as classes de data e hora do Java 8+: [Oracle's Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- Joda-Time, a biblioteca que inspirou `java.time`: [Joda-Time](https://www.joda.org/joda-time/)
